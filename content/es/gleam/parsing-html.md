---
title:                "Análisis de HTML"
date:                  2024-01-20T15:31:37.883515-07:00
html_title:           "Arduino: Análisis de HTML"
simple_title:         "Análisis de HTML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
El análisis (parsing) de HTML es el proceso de convertir texto en una estructura que el programa puede entender y manipular. Los programadores lo hacen para extraer datos, manipular contenido o automatizar tareas en páginas web.

## Cómo hacerlo:
Imaginemos que queremos extraer títulos de un documento HTML. Aquí te dejo un ejemplo simple en Gleam:

```gleam
import gleam/html

let raw_html = """
<html>
<head>
  <title>Ejemplo de HTML</title>
</head>
<body>
  <h1>Bienvendos a Gleam</h1>
  <p>Este es un ejemplo de análisis de HTML.</p>
</body>
</html>
"""

fn main() {
  let doc = html.parse(raw_html)
  let headers = doc |> html.find_all("h1")
  headers |> list.map(fn(h) { h.inner_text() })
}
```

Salida de muestra:

```plaintext
["Bienvendos a Gleam"]
```

## Profundización
Historicamente, analizar HTML era un trabajo pesado y propenso a errores debido a la naturaleza flexible del HTML. Herramientas modernas, como las librerías de parsing de HTML, han simplificado este proceso.

Una alternativa es usar expresiones regulares, pero no es recomendable para documentos complicados o mal formados. Gleam se beneficia de la tipificación estática y manejo seguros, evitando las trampas comunes del parsing.

En cuanto a la implementación, librías de parsing de HTML en Gleam usualmente crean un árbol DOM (Document Object Model) del cual puedes buscar y manipular elementos específicos, tal como mostramos en el ejemplo con la función `html.find_all("h1")`.

## Ver También
- Documentación de Gleam: [https://gleam.run/](https://gleam.run/)
- Tutorial sobre parsing de HTML con Gleam: [Tutorial link]
- Comparación de librerías de parsing de HTML: [Comparison link]