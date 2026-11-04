import random
import subprocess
import time
import tkinter as tk
from pathlib import Path
from tkinter import filedialog, messagebox

RAIZ = Path(__file__).resolve().parent.parent

# ---------- Dimensiones ----------
CW0, CH0 = 780, 580        # tamaño inicial del lienzo (luego se adapta)
PANEL_W = 310              # ancho del panel lateral
MARGEN_X = 80
MARGEN_DER = 110          # espacio a la derecha para la barra de colores
ARRIBA, ABAJO = 105, 65    # márgenes verticales de la escala 0..100
RADIO = 26
MAX_RONDAS = 3000

# ---------- Paleta ----------
FONDO = "#0d0f18"
SUPERFICIE = "#141826"
TARJETA = "#1c2135"
BORDE = "#2a3150"
TEXTO = "#e8eaf6"
TENUE = "#8a90ad"
VERDE = "#2ecc71"
NARANJA = "#f0a030"
AMARILLO = "#f1c40f"
ACENTO = "#5b7cfa"
ROJO = "#e5566d"
BOTON = "#2b3152"

FUENTE = "Helvetica"


# Comunicación directa con HAskell
def ruta_binario():
    r = subprocess.run(["cabal", "list-bin", "grafo-espectral"],
                       cwd=RAIZ, capture_output=True, text=True, check=True)
    return r.stdout.strip()

BINARIO = ruta_binario()

def llamar(comando, matriz, puntajes, fijos):
    entrada = (f"{len(matriz)}\n"
               + "\n".join(" ".join(map(str, f)) for f in matriz) + "\n"
               + " ".join(f"{p:.6f}" for p in puntajes) + "\n"
               + " ".join("1" if f else "0" for f in fijos) + "\n")
    r = subprocess.run([BINARIO, comando], input=entrada,
                       capture_output=True, text=True)
    return interpretar(r.stdout)

def interpretar(texto):
    if texto.startswith("ERROR"):
        raise ValueError(texto[6:].strip())
    cabecera, separador, _ = texto.partition("---DOT---\n")
    if not separador:
        raise ValueError("Respuesta inesperada de Haskell: " + texto[:100])
    datos = {"comp": []}
    for linea in cabecera.strip().splitlines():
        clave, _, valor = linea.partition(" ")
        if clave == "comp":
            datos["comp"].append([int(v) for v in valor.split()])
        elif clave == "estado":
            datos["estado"] = [float(v) for v in valor.split()]
        elif clave == "grados":
            datos["grados"] = [int(v) for v in valor.split()]
        elif clave == "armcomp":
            datos["armcomp"] = [v == "1" for v in valor.split()]
        elif clave in ("armonizado", "verificacion"):
            datos[clave] = valor == "True"
        elif clave == "desarmonia":
            datos[clave] = float(valor)
        else:
            datos[clave] = int(valor)
    return datos


# Da Lectura a archivos
def puntajes_por_defecto(n):
    if n == 5:
        return [10.0, 20.0, 30.0, 0.0, 100.0]
    rnd = random.Random(7)
    return [round(rnd.uniform(0, 100), 1) for _ in range(n)]

def leer_grafo(texto):
    # Devuelve (matriz, puntajes). La línea de puntajes es opcional
    lineas = [l for l in texto.strip().splitlines() if l.strip()]
    n = int(lineas[0])
    matriz = [[int(v) for v in l.split()] for l in lineas[1:n + 1]]
    puntajes = None
    if len(lineas) > n + 1:
        p = [float(v) for v in lineas[n + 1].split()]
        if len(p) == n:
            puntajes = p
    return matriz, puntajes or puntajes_por_defecto(n)

# Solo para efectos visuales
def hex_a_rgb(h):
    return tuple(int(h[i:i + 2], 16) for i in (1, 3, 5))

def mezclar(c1, c2, t):
    """Mezcla dos colores hex. t=0 -> c1, t=1 -> c2."""
    a, b = hex_a_rgb(c1), hex_a_rgb(c2)
    return "#%02x%02x%02x" % tuple(round(a[k] + (b[k] - a[k]) * t) for k in range(3))

def color_puntaje(p):
    t = max(0, min(1, p / 100))
    return "#%02x50%02x" % (round(255 * t), round(255 * (1 - t)))

def rect_redondo(c, x0, y0, x1, y1, r, **kw):
    r = max(1, min(r, (x1 - x0) / 2, (y1 - y0) / 2))
    pts = [x0 + r, y0, x1 - r, y0, x1, y0, x1, y0 + r,
           x1, y1 - r, x1, y1, x1 - r, y1, x0 + r, y1,
           x0, y1, x0, y1 - r, x0, y0 + r, x0, y0]
    return c.create_polygon(pts, smooth=True, **kw)


# Objetos construidos para GUI
class Boton(tk.Canvas):
    def __init__(self, padre, texto, comando, color=BOTON, alto=38,
                 fondo=SUPERFICIE, fuente=(FUENTE, 10, "bold")):
        super().__init__(padre, width=10, height=alto, bg=fondo,
                         highlightthickness=0, bd=0, cursor="hand2")
        self.texto, self.comando, self.color = texto, comando, color
        self.fondo, self.fuente = fondo, fuente
        self.estado, self.encima, self.presionado = "normal", False, False
        self.bind("<Configure>", lambda e: self.dibujar())
        self.bind("<Enter>", self._entrar)
        self.bind("<Leave>", self._salir)
        self.bind("<ButtonPress-1>", self._presionar)
        self.bind("<ButtonRelease-1>", self._soltar)

    def set_texto(self, texto):
        if texto != self.texto:
            self.texto = texto
            self.dibujar()

    def set_estado(self, estado):
        if estado != self.estado:
            self.estado = estado
            self.config(cursor="hand2" if estado == "normal" else "arrow")
            self.dibujar()

    def _entrar(self, _):
        self.encima = True
        self.dibujar()

    def _salir(self, _):
        self.encima = False
        self.presionado = False
        self.dibujar()

    def _presionar(self, _):
        if self.estado == "normal":
            self.presionado = True
            self.dibujar()

    def _soltar(self, e):
        activo = self.presionado
        self.presionado = False
        self.dibujar()
        dentro = 0 <= e.x <= self.winfo_width() and 0 <= e.y <= self.winfo_height()
        if activo and dentro and self.estado == "normal":
            self.comando()

    def dibujar(self):
        self.delete("all")
        w, h = self.winfo_width(), self.winfo_height()
        if w < 8:
            return
        if self.estado == "disabled":
            relleno = mezclar(self.fondo, self.color, 0.28)
            texto = "#5d6482"
        else:
            relleno = self.color
            if self.presionado:
                relleno = mezclar(self.color, "#000000", 0.18)
            elif self.encima:
                relleno = mezclar(self.color, "#ffffff", 0.14)
            texto = "white"
        dy = 1 if self.presionado else 0
        rect_redondo(self, 1, 3, w - 1, h - 1, 12,
                     fill=mezclar(self.fondo, "#000000", 0.45), outline="")   # sombra
        rect_redondo(self, 1, 1 + dy, w - 1, h - 3 + dy, 12,
                     fill=relleno, outline=mezclar(relleno, "#ffffff", 0.10))
        self.create_text(w / 2, (h - 2) / 2 + dy, text=self.texto,
                         fill=texto, font=self.fuente)


class Deslizador(tk.Canvas):
    # Control deslizante con valores enteros
    def __init__(self, padre, minimo, maximo, valor, al_cambiar=None, fondo=SUPERFICIE):
        super().__init__(padre, width=10, height=28, bg=fondo,
                         highlightthickness=0, bd=0, cursor="hand2")
        self.min, self.max, self.valor = minimo, maximo, valor
        self.al_cambiar = al_cambiar
        self.bind("<Configure>", lambda e: self.dibujar())
        self.bind("<Button-1>", self._mover)
        self.bind("<B1-Motion>", self._mover)

    def get(self):
        return self.valor

    def _x_de(self, v):
        w = self.winfo_width()
        return 12 + (v - self.min) / (self.max - self.min) * (w - 24)

    def _mover(self, e):
        w = self.winfo_width()
        t = (e.x - 12) / max(1, w - 24)
        v = max(self.min, min(self.max, round(self.min + t * (self.max - self.min))))
        if v != self.valor:
            self.valor = v
            self.dibujar()
            if self.al_cambiar:
                self.al_cambiar(v)

    def dibujar(self):
        self.delete("all")
        w = self.winfo_width()
        if w < 30:
            return
        y = 14
        self.create_line(12, y, w - 12, y, width=6, fill=BORDE, capstyle="round")
        xk = self._x_de(self.valor)
        self.create_line(12, y, xk, y, width=6, fill=ACENTO, capstyle="round")
        for v in range(self.min, self.max + 1):
            x = self._x_de(v)
            self.create_oval(x - 1.5, y - 1.5, x + 1.5, y + 1.5,
                             fill=mezclar(BORDE, "#ffffff", 0.25), outline="")
        self.create_oval(xk - 10, y - 8, xk + 10, y + 12,
                         fill=mezclar(SUPERFICIE, "#000000", 0.4), outline="")
        self.create_oval(xk - 9, y - 9, xk + 9, y + 9, fill="white",
                         outline=ACENTO, width=3)


class Tarjeta(tk.Canvas):
    # Tarjeta con titulo pequeño solo visual
    def __init__(self, padre, titulo, alto=48, fondo=SUPERFICIE):
        super().__init__(padre, width=10, height=alto, bg=fondo,
                         highlightthickness=0, bd=0)
        self.titulo, self.valor = titulo, "-"
        self.bind("<Configure>", lambda e: self.dibujar())

    def set(self, valor):
        valor = str(valor)
        if valor != self.valor:
            self.valor = valor
            self.dibujar()

    def dibujar(self):
        self.delete("all")
        w, h = self.winfo_width(), self.winfo_height()
        if w < 8:
            return
        rect_redondo(self, 1, 1, w - 1, h - 1, 12, fill=TARJETA, outline=BORDE)
        self.create_text(w / 2, 14, text=self.titulo, fill=TENUE, font=(FUENTE, 8))
        self.create_text(w / 2, h - 17, text=self.valor, fill=TEXTO,
                         font=(FUENTE, 15, "bold"))


class Pastilla(tk.Canvas):
    # Encapsula con con color, para estados 
    def __init__(self, padre, alto, punto=False, fuente=(FUENTE, 9, "bold"),
                 fondo=SUPERFICIE):
        super().__init__(padre, width=10, height=alto, bg=fondo,
                         highlightthickness=0, bd=0)
        self.punto, self.fuente, self.fondo = punto, fuente, fondo
        self.texto, self.color = "", TENUE
        self.bind("<Configure>", lambda e: self.dibujar())

    def set(self, texto, color):
        if (texto, color) != (self.texto, self.color):
            self.texto, self.color = texto, color
            self.dibujar()

    def dibujar(self):
        self.delete("all")
        w, h = self.winfo_width(), self.winfo_height()
        if w < 8:
            return
        rect_redondo(self, 1, 1, w - 1, h - 1, min(14, h / 2),
                     fill=mezclar(self.fondo, self.color, 0.13),
                     outline=mezclar(self.fondo, self.color, 0.50))
        x = 16
        if self.punto:
            self.create_oval(14, h / 2 - 5, 24, h / 2 + 5, fill=self.color, outline="")
            x = 34
        self.create_text(x, h / 2, text=self.texto, anchor="w", width=w - x - 10,
                         fill=mezclar(self.color, "#ffffff", 0.55), font=self.fuente)


class InfoNodo(tk.Canvas):
    # Tarjeta con los datos del nodo
    def __init__(self, padre, alto=84, fondo=SUPERFICIE):
        super().__init__(padre, width=10, height=alto, bg=fondo,
                         highlightthickness=0, bd=0)
        self.contenido = None
        self.bind("<Configure>", lambda e: self.dibujar())

    def set(self, contenido):
        if contenido != self.contenido:
            self.contenido = contenido
            self.dibujar()

    def dibujar(self):
        self.delete("all")
        w, h = self.winfo_width(), self.winfo_height()
        if w < 8:
            return
        rect_redondo(self, 1, 1, w - 1, h - 1, 12, fill=TARJETA, outline=BORDE)
        if self.contenido is None:
            self.create_text(w / 2, h / 2, text="Pasa el mouse sobre un nodo\npara ver sus datos",
                             fill=TENUE, font=(FUENTE, 9), justify="center")
            return
        titulo, lineas = self.contenido
        self.create_text(14, 16, text=titulo, anchor="w", fill=TEXTO,
                         font=(FUENTE, 10, "bold"))
        for k, l in enumerate(lineas):
            self.create_text(14, 34 + k * 15, text=l, anchor="w", fill=TENUE,
                             font=(FUENTE, 9), width=w - 28)


# MAIN del "juego" / Programa
class Juego:
    def __init__(self, raiz, matriz, puntajes):
        self.raiz = raiz
        raiz.title("Armonizador de grafos")
        raiz.configure(bg=SUPERFICIE)
        self.cw, self.ch = CW0, CH0
        self.pantalla_completa = False
        raiz.minsize(960, 700)
        raiz.grid_columnconfigure(0, weight=1)   # el lienzo se estira
        raiz.grid_rowconfigure(0, weight=1)

        self.canvas = tk.Canvas(raiz, width=CW0, height=CH0, bg=FONDO,
                                highlightthickness=0)
        self.canvas.grid(row=0, column=0, sticky="nsew")

        self.lateral = tk.Frame(raiz, bg=SUPERFICIE, width=PANEL_W)
        self.lateral.grid(row=0, column=1, sticky="ns")
        self.lateral.pack_propagate(False)
        self.construir_panel()

        self.canvas.bind("<Button-1>", self.al_presionar)
        self.canvas.bind("<B1-Motion>", self.al_mover)
        self.canvas.bind("<ButtonRelease-1>", self.al_soltar)
        self.canvas.bind("<Button-3>", self.al_clic_derecho)
        self.canvas.bind("<Button-2>", self.al_clic_derecho)   # macOS
        self.canvas.bind("<Motion>", self.al_pasar)
        self.canvas.bind("<Leave>", lambda e: self.poner_hover(None))
        raiz.bind("<space>", lambda e: self.alternar_auto())
        raiz.bind("<F11>", lambda e: self.alternar_pantalla_completa())
        raiz.bind("<Escape>", lambda e: self.salir_pantalla_completa())

        self.cargar(matriz, puntajes)
        self.canvas.bind("<Configure>", self.al_cambiar_tamano)
        self.maximizar()

    # Tamaño de la ventana
    def maximizar(self):
        try:
            self.raiz.state("zoomed")                  # Windows
        except tk.TclError:
            try:
                self.raiz.attributes("-zoomed", True)  # Linux
            except tk.TclError:
                pass

    def alternar_pantalla_completa(self):
        self.pantalla_completa = not self.pantalla_completa
        self.raiz.attributes("-fullscreen", self.pantalla_completa)

    def salir_pantalla_completa(self):
        if self.pantalla_completa:
            self.pantalla_completa = False
            self.raiz.attributes("-fullscreen", False)

    def al_cambiar_tamano(self, e):
        if (e.width, e.height) == (self.cw, self.ch):
            return
        self.cw, self.ch = e.width, e.height
        self.calcular_posiciones_x()
        self.dibujar()

    # Configuración del panel lateral
    def seccion(self, texto, arriba=10):
        tk.Label(self.lateral, text=texto, bg=SUPERFICIE, fg=TENUE,
                 font=(FUENTE, 8, "bold")).pack(anchor="w", padx=18, pady=(arriba, 5))

    def construir_panel(self):
        p = self.lateral

        tk.Label(p, text="Armonizador", bg=SUPERFICIE, fg=TEXTO,
                 font=(FUENTE, 20, "bold")).pack(anchor="w", padx=18, pady=(18, 0))
        tk.Label(p, text="Consenso en grafos con la Laplaciana", bg=SUPERFICIE,
                 fg=TENUE, font=(FUENTE, 9)).pack(anchor="w", padx=18)

        # Estado
        self.estado = Pastilla(p, 54, punto=True)
        self.estado.pack(fill="x", padx=16, pady=(12, 10))

        # Botones
        self.btn_auto = Boton(p, "Armonizar", self.alternar_auto, ACENTO, alto=44,
                              fuente=(FUENTE, 11, "bold"))
        self.btn_auto.pack(fill="x", padx=16)

        fila = tk.Frame(p, bg=SUPERFICIE)
        fila.pack(fill="x", padx=16, pady=(8, 0))
        self.btn_paso = Boton(fila, "Un paso", self.paso_manual)
        self.btn_paso.pack(side="left", expand=True, fill="x", padx=(0, 4))
        Boton(fila, "Reiniciar", self.reiniciar).pack(
            side="left", expand=True, fill="x", padx=(4, 0))

        fila2 = tk.Frame(p, bg=SUPERFICIE)
        fila2.pack(fill="x", padx=16, pady=(8, 0))
        Boton(fila2, "Aleatorio", self.aleatorizar).pack(
            side="left", expand=True, fill="x", padx=(0, 4))
        Boton(fila2, "Cargar", self.cargar_archivo).pack(
            side="left", expand=True, fill="x", padx=(4, 0))

        # Velocidad
        cab = tk.Frame(p, bg=SUPERFICIE)
        cab.pack(fill="x", padx=18, pady=(12, 0))
        tk.Label(cab, text="VELOCIDAD", bg=SUPERFICIE, fg=TENUE,
                 font=(FUENTE, 8, "bold")).pack(side="left")
        self.vel_valor = tk.Label(cab, text="4 / 10", bg=SUPERFICIE, fg=TEXTO,
                                  font=(FUENTE, 9, "bold"))
        self.vel_valor.pack(side="right")
        self.velocidad = Deslizador(
            p, 1, 10, 4, al_cambiar=lambda v: self.vel_valor.config(text=f"{v} / 10"))
        self.velocidad.pack(fill="x", padx=16, pady=(2, 0))

        # Tarjetas de datos
        self.seccion("DATOS DEL GRAFO")
        rejilla = tk.Frame(p, bg=SUPERFICIE)
        rejilla.pack(fill="x", padx=14)
        self.datos = {}
        for k, (clave, titulo) in enumerate([
                ("nodos", "Nodos"), ("aristas", "Aristas"), ("componentes", "Componentes"),
                ("traza", "Traza de L"), ("rango", "Rango de L"), ("ronda", "Ronda")]):
            t = Tarjeta(rejilla, titulo)
            t.grid(row=k // 3, column=k % 3, padx=2, pady=2, sticky="nsew")
            rejilla.grid_columnconfigure(k % 3, weight=1, uniform="c")
            self.datos[clave] = t

        self.verif = Pastilla(p, 30, fuente=(FUENTE, 9, "bold"))
        self.verif.pack(fill="x", padx=16, pady=(8, 0))
        self.desarm = tk.Label(p, text="", bg=SUPERFICIE, fg=TENUE,
                               font=(FUENTE, 9), anchor="w")
        self.desarm.pack(fill="x", padx=18, pady=(6, 0))

        # Nodo bajo el mouse
        self.seccion("NODO BAJO EL MOUSE", arriba=8)
        self.info_nodo = InfoNodo(p, 76)
        self.info_nodo.pack(fill="x", padx=16, pady=(0, 14))

    # Para cargar un grafo
    def cargar(self, matriz, puntajes):
        n = len(matriz)
        info = llamar("analizar", matriz, puntajes, [False] * n)   # Haskell valida
        self.matriz, self.n = matriz, n
        self.inicial = list(puntajes)
        self.puntajes = list(puntajes)
        self.fijos = [False] * n
        self.info = info
        self.comps = info["comp"]
        self.auto = False
        self.btn_auto.set_texto("▶  Armonizar")
        self.ronda = 0
        self.arrastrado = None
        self.hover = None
        self.ultimo_refresco = 0.0
        self.calcular_posiciones_x()
        self.dibujar()

    def cargar_archivo(self):
        ruta = filedialog.askopenfilename(initialdir=RAIZ / "data",
                                          filetypes=[("Grafo", "*.txt")])
        if not ruta:
            return
        try:
            matriz, puntajes = leer_grafo(Path(ruta).read_text())
            self.cargar(matriz, puntajes)
        except (ValueError, IndexError) as e:
            messagebox.showerror("Grafo inválido", str(e))

    # Geometria para mostrar
    def calcular_posiciones_x(self):
        orden = []
        for k, c in enumerate(self.comps):
            orden += c + ([None] if k < len(self.comps) - 1 else [])
        paso = (self.cw - MARGEN_X - MARGEN_DER) / max(1, len(orden) - 1)
        self.x = {n: MARGEN_X + pos * paso
                  for pos, n in enumerate(orden) if n is not None}

    def y_de(self, puntaje):
        return self.ch - ABAJO - puntaje / 100 * (self.ch - ARRIBA - ABAJO)

    def puntaje_de(self, y):
        p = (self.ch - ABAJO - y) / (self.ch - ARRIBA - ABAJO) * 100
        return max(0.0, min(100.0, p))

    # Dibuja directamente el grafo
    def dibujar(self):
        c = self.canvas
        c.delete("all")

        # Líneas guía
        for v in (0, 25, 50, 75, 100):
            y = self.y_de(v)
            c.create_line(MARGEN_X - 40, y, self.cw - 56, y, fill="#1b1f30", dash=(2, 6))
            c.create_text(26, y, text=str(v), fill=TENUE, font=(FUENTE, 9))

        # Leyenda de colores (barra vertical a la derecha)
        y_alto, y_bajo = self.y_de(100), self.y_de(0)
        pasos = 50
        alto = (y_bajo - y_alto) / pasos
        for k in range(pasos):
            c.create_rectangle(self.cw - 34, y_alto + k * alto, self.cw - 22,
                               y_alto + (k + 1) * alto + 1,
                               fill=color_puntaje(100 - k * 100 / pasos), outline="")
        c.create_text(self.cw - 28, y_alto - 12, text="alto", fill=TENUE, font=(FUENTE, 8))
        c.create_text(self.cw - 28, y_bajo + 12, text="bajo", fill=TENUE, font=(FUENTE, 8))

        # Tarjeta por componente
        for k, comp in enumerate(self.comps):
            ok = self.info["armcomp"][k]
            x0 = min(self.x[i] for i in comp) - RADIO - 18
            x1 = max(self.x[i] for i in comp) + RADIO + 18
            rect_redondo(c, x0, 52, x1, self.ch - 28, 16,
                         fill=mezclar(FONDO, VERDE, 0.07) if ok else "#121524",
                         outline=VERDE if ok else BORDE, width=2 if ok else 1)
            c.create_text((x0 + x1) / 2, 70,
                          text=f"Componente {k + 1}" + ("  ✔" if ok else ""),
                          fill=VERDE if ok else TENUE, font=(FUENTE, 10, "bold"))

        # Aristas: el color pasa de verde (equilibrio) a naranja (tensión)
        for i in range(self.n):
            for j in range(i + 1, self.n):
                if self.matriz[i][j] == 1:
                    dif = abs(self.puntajes[i] - self.puntajes[j])
                    t = min(1.0, dif / 25)
                    c.create_line(self.x[i], self.y_de(self.puntajes[i]),
                                  self.x[j], self.y_de(self.puntajes[j]),
                                  fill=mezclar(VERDE, NARANJA, t),
                                  width=2 + round(2 * t), capstyle="round")

        # Nodos
        comp_de = {i: k for k, comp in enumerate(self.comps) for i in comp}
        for i in range(self.n):
            cx, cy = self.x[i], self.y_de(self.puntajes[i])
            col = color_puntaje(self.puntajes[i])
            ok = self.info["armcomp"][comp_de[i]]

            for r, t in ((RADIO + 12, 0.85), (RADIO + 7, 0.7)):   # resplandor
                c.create_oval(cx - r, cy - r, cx + r, cy + r,
                              fill=mezclar(col, FONDO, t), outline="")
            if self.fijos[i]:
                c.create_oval(cx - RADIO - 8, cy - RADIO - 8,
                              cx + RADIO + 8, cy + RADIO + 8,
                              outline=AMARILLO, width=2, dash=(4, 3))
                c.create_text(cx, cy + RADIO + 20, text="📌 FIJO", fill=AMARILLO,
                              font=(FUENTE, 8, "bold"))
            borde = "white" if i == self.hover else (VERDE if ok else "#c9cde6")
            c.create_oval(cx - RADIO, cy - RADIO, cx + RADIO, cy + RADIO,
                          fill=col, outline=borde,
                          width=4 if i == self.hover else 3)
            c.create_text(cx, cy - 6, text=str(i), fill="white",
                          font=(FUENTE, 12, "bold"))
            c.create_text(cx, cy + 9, text=f"{self.puntajes[i]:.1f}",
                          fill="white", font=(FUENTE, 9))

        # Cartel de armonía
        if self.info["armonizado"]:
            rect_redondo(c, self.cw / 2 - 200, 8, self.cw / 2 + 200, 40, 14,
                         fill="#12301f", outline=VERDE, width=2)
            c.create_text(self.cw / 2, 24,
                          text=("✔  GRAFO ARMONIZADO" if self.ronda == 0 else
                                f"✔  GRAFO ARMONIZADO en {self.ronda} rondas"),
                          fill=VERDE, font=(FUENTE, 11, "bold"))

        # Ayuda de controles
        c.create_text(self.cw / 2, self.ch - 12,
                      text="Arrastrar nodo: cambiar puntaje   ·   Clic derecho: fijar / soltar"
                           "   ·   Espacio: pausa   ·   F11: pantalla completa",
                      fill="#4b5170", font=(FUENTE, 8))

        self.actualizar_panel()

    # Panel
    def actualizar_panel(self):
        i = self.info
        for clave, valor in (("nodos", i["nodos"]), ("aristas", i["aristas"]),
                             ("componentes", i["componentes"]),
                             ("traza", i["traza"]), ("rango", i["rango"]),
                             ("ronda", self.ronda)):
            self.datos[clave].set(valor)

        ok = i["verificacion"]
        self.verif.set("✔  n − rango = componentes" if ok
                       else "✘  n − rango ≠ componentes", VERDE if ok else ROJO)
        self.desarm.config(text=f"Desarmonía actual: {i['desarmonia']:.4f}")

        h = self.hover
        if h is None:
            self.info_nodo.set(None)
        else:
            vec = [j for j in range(self.n) if self.matriz[h][j] == 1]
            self.info_nodo.set((
                f"Nodo {h}  ·  {'FIJO' if self.fijos[h] else 'libre'}",
                [f"Puntaje: {self.puntajes[h]:.2f}",
                 f"Grado: {i['grados'][h]}",
                 f"Vecinos: {vec if vec else 'ninguno'}"]))

        listo = i["armonizado"]
        self.btn_auto.set_estado("disabled" if listo else "normal")
        self.btn_paso.set_estado("disabled" if listo else "normal")
        if listo:
            msg, col = "Armonía alcanzada. Mueve o fija un nodo para romperla.", VERDE
        elif self.auto:
            msg, col = "Armonizando…", AMARILLO
        elif self.ronda > 0:
            msg, col = "En pausa. Pulsa Armonizar o la barra espaciadora.", NARANJA
        else:
            msg, col = "Arrastra nodos para cambiar sus puntajes y pulsa Armonizar.", ACENTO
        self.estado.set(msg, col)

    # Logica de llamado de Haskell
    def refrescar(self):
        self.info = llamar("analizar", self.matriz, self.puntajes, self.fijos)

    def paso_manual(self):
        self.detener_auto()
        self.un_paso()

    def un_paso(self):
        # Pide "UNA ronda a Haskell", salvo que ya esté armonizado
        if self.info["armonizado"]:
            self.detener_auto()
            return True
        d = llamar("paso", self.matriz, self.puntajes, self.fijos)
        self.puntajes = d["estado"]
        self.info = d
        self.ronda += 1
        self.dibujar()
        return d["armonizado"]

    # Animación que hace
    def retardo(self):
        return int(600 - 55 * self.velocidad.get())

    def alternar_auto(self):
        if self.auto:
            self.detener_auto()
        else:
            self.iniciar_auto()
        self.actualizar_panel()

    def iniciar_auto(self):
        if self.auto or self.info["armonizado"]:
            return
        self.auto = True
        self.btn_auto.set_texto("⏸  Pausar")
        self.actualizar_panel()
        self.raiz.after(self.retardo(), self.tick)

    def detener_auto(self):
        self.auto = False
        self.btn_auto.set_texto("▶  Armonizar")

    def tick(self):
        if not self.auto:
            return
        terminado = self.un_paso()
        if terminado or self.ronda >= MAX_RONDAS:
            self.detener_auto()
            self.actualizar_panel()
        else:
            self.raiz.after(self.retardo(), self.tick)

    # Mouse
    def nodo_en(self, px, py):
        for i in range(self.n):
            if (px - self.x[i]) ** 2 + (py - self.y_de(self.puntajes[i])) ** 2 <= RADIO ** 2:
                return i
        return None

    def poner_hover(self, i):
        if i != self.hover:
            self.hover = i
            self.dibujar()

    def al_pasar(self, e):
        if self.arrastrado is None:
            self.poner_hover(self.nodo_en(e.x, e.y))
            self.canvas.config(cursor="hand2" if self.hover is not None else "")

    def nueva_perturbacion(self):
        # Tras mover algo a mano: reanaliza y reinicia el contador de rondas
        self.refrescar()
        self.ronda = 0
        self.dibujar()

    def al_presionar(self, e):
        self.detener_auto()
        self.arrastrado = self.nodo_en(e.x, e.y)
        if self.arrastrado is not None:
            self.hover = self.arrastrado

    def al_mover(self, e):
        if self.arrastrado is None:
            return
        self.puntajes[self.arrastrado] = self.puntaje_de(e.y)
        ahora = time.monotonic()
        if ahora - self.ultimo_refresco > 0.05:      # máx. ~20 llamadas/seg
            self.refrescar()
            self.ultimo_refresco = ahora
        self.dibujar()

    def al_soltar(self, _):
        if self.arrastrado is None:
            return
        self.arrastrado = None
        self.nueva_perturbacion()     # solo reanaliza; la armonización la inicia el usuario

    def al_clic_derecho(self, e):
        i = self.nodo_en(e.x, e.y)
        if i is None:
            return
        self.detener_auto()
        self.fijos[i] = not self.fijos[i]
        self.nueva_perturbacion()

    # Botones
    def reiniciar(self):
        self.detener_auto()
        self.puntajes = list(self.inicial)
        self.fijos = [False] * self.n
        self.nueva_perturbacion()

    def aleatorizar(self):
        self.detener_auto()
        self.puntajes = [round(random.uniform(0, 100), 1) for _ in range(self.n)]
        self.nueva_perturbacion()


if __name__ == "__main__":
    matriz, puntajes = leer_grafo((RAIZ / "data" / "grafo.txt").read_text()) #Inicia con un precagado.
    raiz = tk.Tk()
    Juego(raiz, matriz, puntajes)
    raiz.mainloop()