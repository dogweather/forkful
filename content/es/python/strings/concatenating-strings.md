---
title:                "Concatenación de cadenas de texto"
aliases: - /es/python/concatenating-strings.md
date:                  2024-01-20T17:35:40.514603-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenación de cadenas de texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué & Para Qué?
La concatenación de cadenas es el proceso de unir dos o más cadenas de texto en una sola. Los programadores concatenan cadenas para formar mensajes, construir URLs, o cuando necesitan unir información textual de diferentes fuentes.

## Cómo:
```Python
# Concatenar con el operador +
saludo = "Hola, " + "mundo!"
print(saludo)

# Concatenar con la función join()
nombres = ["Alice", "Bob", "Charlie"]
lista_nombres = ", ".join(nombres)
print("Amigos: " + lista_nombres)

# Concatenar con f-strings (Python 3.6 en adelante)
nombre = "Mundo"
mensaje = f"Hola, {nombre}!"
print(mensaje)
```

Salida:
```
Hola, mundo!
Amigos: Alice, Bob, Charlie
Hola, Mundo!
```

## Deep Dive:
Históricamente, la concatenación de cadenas era una operación costosa en términos de rendimiento. Cada concatenación significaba crear una nueva cadena, ya que las cadenas son inmutables en Python. Sin embargo, las modernas implementaciones de Python suelen optimizar la concatenación de cadenas pequeñas con operaciones como las vistas en el ejemplo, con el fin de mejorar el rendimiento.

Alternativas como la función `join()` y las f-strings no sólo ofrecen un estilo más claro y conciso para concatenar, sino que también pueden ser más eficientes, sobre todo al unir una lista grande de cadenas.

En cuanto a la implementación, vale la pena mencionar que al utilizar `+`, Python tiene que recalcular el tamaño de la nueva cadena resultante y copiar las cadenas originales a la nueva ubicación en memoria. `join()` es más eficiente en este sentido, ya que calcula el tamaño total una sola vez, y luego realiza la concatenación. Las f-strings, introducidas en Python 3.6, internamente utilizan un mecanismo similar a `format()`, lo que las hace muy rápidas y legibles.

## See Also:
- Documentación oficial de Python sobre las [f-strings](https://docs.python.org/3/reference/lexical_analysis.html#f-strings)
- Una comparación de rendimiento: [Python String Concatenation](https://waymoot.org/home/python_string/)
- Para profundizar en los temas de optimización, te recomiendo leer ["High Performance Python" de Micha Gorelick y Ian Ozsvald](https://www.oreilly.com/library/view/high-performance-python/9781449361747/).
