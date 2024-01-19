---
title:                "Descargando una página web"
html_title:           "Arduino: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Descargar una página web es el proceso de extraer toda la información contenida en ella usando código. Los programadores lo hacen para manipular los datos, realizar web scraping o pruebas entre otros fines.

## ¿Cómo hacerlo?

Para descargar una página web en Kotlin, usaremos la biblioteca Jsoup. Primero, agrégala a tu proyecto con Gradle:

```Kotlin
dependencies {
  implementation 'org.jsoup:jsoup:1.13.1'
}
```

Ahora veamos cómo descargar una página web:

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val doc = Jsoup.connect("http://example.com").get()
    val title = doc.title()
    println("Título: $title")
}
```

Lo que obtienes es el título de la página que se encuentra en el tag `<title>`. Es un ejemplo básico, pero Jsoup puede hacer mucho más.

## Inmersión profunda

Originalmente, la descarga de páginas web se realizaba con rutinas de bajo nivel que manejaban directamente las conexiones HTTP. Ahora con bibliotecas como Jsoup, es mucho más fácil.

Además de Jsoup, hay otras opciones como khttp y java.net, aunque Jsoup es la más completa en términos de manejo de HTML y CSS.

El método `connect()`, devuelve un objeto `Connection`, que luego se convierte en un `Document` al llamar a `get()`. Jsoup se encarga de todas las intrincaciones del manejo de conexiones HTTP y el parseo del HTML.

## Ver también

Para más información, revisa la [documentación oficial de Jsoup](https://jsoup.org/). Para conocer más opciones, puedes revisar [khttp](http://khttp.readthedocs.io/en/latest/) y la [API de java.net](https://docs.oracle.com/javase/8/docs/api/java/net/package-summary.html).