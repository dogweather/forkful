---
title:                "Descargando una página web"
html_title:           "Elixir: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Descargar una página web es simplemente obtener el contenido de una página de internet y guardarlo en nuestra computadora o dispositivo. Los programadores lo hacen para acceder y manipular datos de la web, como por ejemplo, obtener información de una página de noticias o de una red social.

## Cómo hacerlo:

Para descargar una página web en Elixir, primero debemos importar el módulo ```HTTPoison``` que nos permitirá realizar solicitudes HTTP. A continuación, utilizando la función ```HTTPoison.get```, podemos especificar la URL de la página que queremos descargar.

```Elixir
import HTTPoison

page = HTTPoison.get("http://www.miweb.com")
```

El resultado de la función será un objeto de tipo ```HTTPoison.Response``` que contiene el contenido de la página web que hemos descargado. Para acceder a este contenido, podemos utilizar la función ```HTTPoison.body```.

```Elixir
content = HTTPoison.body(page)
```

Finalmente, podemos imprimir el contenido de la página en la consola utilizando la función ```IO.puts```.

```Elixir
IO.puts(content)
```

## Exploración en profundidad:

Descargar páginas web se ha vuelto una tarea cada vez más común para los programadores, ya que muchas aplicaciones y servicios dependen de la información que se encuentra en internet. Además, con el avance de tecnologías como el web scraping, se ha vuelto más fácil y común obtener y manipular datos de forma automatizada.

Existen alternativas a usar el módulo ```HTTPoison``` para descargar páginas web, como por ejemplo, ```HTTPotion``` o el módulo nativo ```HTTP```. Sin embargo, ```HTTPoison``` ha demostrado ser muy eficiente y sencillo de utilizar en comparación con las otras opciones.

En cuanto a la implementación del módulo ```HTTPoison```, utiliza la librería ```hackney``` para realizar las solicitudes HTTP y tiene una amplia documentación disponible en línea.

## Ver también:

- [Documentación oficial de HTTPoison](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Web scraping con Elixir y Floki](https://medium.com/@jlouis666/web-scraping-with-elixir-floki-597f816b2e81)
- [Alternativas a HTTPoison](https://erlangcentral.org/?p=1048)