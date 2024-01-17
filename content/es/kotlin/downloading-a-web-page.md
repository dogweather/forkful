---
title:                "Descargando una página web"
html_title:           "Kotlin: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¡Descargar una página web en Kotlin!

## ¿Qué y Porqué?

Descargar una página web significa obtener todo el contenido de una página web específica y guardarlo en tu dispositivo. Los programadores hacen esto para realizar tareas como visualizar información en un formato legible o extraer datos importantes de una página web.

## Cómo hacerlo:

```Kotlin
val url = "https://www.ejemplo.com"
val inputStream = URL(url).openStream()
val text = BufferedReader(InputStreamReader(inputStream)).readText()
println(text)
```

Salida:
Esta es una página web de ejemplo.

## Profundizando:

Descargar páginas web es una técnica común en el mundo de la programación, especialmente para proyectos relacionados con la web. Hay varias formas de hacerlo, como utilizando bibliotecas externas o haciendo una solicitud HTTP directamente. En Kotlin, podemos usar la clase ```URL``` para crear un objeto URL y pasar esto a la función ```openStream()``` para obtener un ```InputStream``` que luego podemos leer con una clase como ```BufferedReader```.

## Ver también:

- [Documentación de Kotlin para la clase URL](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-u-r-l/index.html)
- [Artículo sobre cómo descargar páginas web en Kotlin](https://simplecoding.org/download-web-page-in-kotlin/)
- [Código de ejemplo en GitHub sobre cómo descargar páginas web en Kotlin](https://github.com/username/example-code/blob/master/download-web-page.kt)