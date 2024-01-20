---
title:                "Descargando una página web"
html_title:           "Arduino: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Descargar una página web implica traer los datos de esa página desde un servidor a tu ordenador. Los programadores hacen esto para interactuar con el contenido web, ya sea para análisis de datos, pruebas o automatización.

## Cómo hacerlo:

Aquí te muestro un ejemplo utilizando la biblioteca JSoup. Primero, necesitarás agregar la dependencia de JSoup en tu archivo pom.xml si estás usando Maven.

```Java
<dependency>
    <groupId>org.jsoup</groupId>
    <artifactId>jsoup</artifactId>
    <version>1.12.1</version>
</dependency>
```

Ahora, puedes descargar una página web de la siguiente manera:

```Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class WebDownloader {
    public static void main(String[] args) throws Exception {
        String url = "http://www.example.com";
        Document document = Jsoup.connect(url).get();
        System.out.println(document.html());
    }
}
```

El resultado será el HTML completo de la página "http://www.example.com".

## Inmersión profunda

La descarga de páginas web ha sido una herramienta esencial para los programadores desde los primeros días del internet. Hoy en día, se utiliza en muchas áreas, incluyendo la minería de datos, las pruebas de aplicaciones web y la automatización.

Los métodos alternativos para descargar una página web incluyen el uso de otras bibliotecas de Java como HttpClient de Apache o la API de HttpURLConnection incorporada en Java. Cada una tiene sus propios pros y contras en términos de facilidad de uso y flexibilidad.

Detalles de implementación: El método Jsoup.connect() establece una conexión con la URL dada, y el método get() envía una solicitud GET a dicha dirección. Esto trae la página completa, que luego puede ser adoptada y manipulada.

## Ver también

1. [Documentación de JSoup](https://jsoup.org/)
3. [Clase HttpURLConnection de Java](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)