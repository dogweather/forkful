---
title:                "Enviando una solicitud http"
html_title:           "Elixir: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado cómo funciona la comunicación entre tu navegador y un servidor web? Bueno, eso es posible gracias a las solicitudes HTTP. En este artículo, aprenderemos cómo enviar una solicitud HTTP utilizando Elixir.

## Cómo hacerlo

```Elixir
# Primero, importa el módulo HTTPoison en tu archivo
# Esto nos permitirá hacer solicitudes HTTP
import HTTPoison

# Luego, utiliza la función get/2 para enviar una solicitud GET
# Le pasamos la URL como primer argumento y un mapa de opciones como segundo argumento
# En este caso, estamos solicitando la página principal de "Google"
response = HTTPoison.get("https://www.google.com/", [])

# Por último, podemos acceder al código de estado y al cuerpo de la respuesta utilizando la función split/1
# El código de estado nos indicará si la solicitud fue exitosa o no
# El cuerpo de la respuesta es el contenido de la página solicitada
status_code = response |> HTTPoison.split() |> List.first()
body = response |> HTTPoison.split() |> List.last()
```

Una vez que hayamos obtenido la respuesta, podemos trabajar con ella como queramos. Podemos mostrar el código de estado, extraer información del cuerpo de la respuesta o incluso guardar la página en un archivo para acceder a ella más tarde.

## Inmersión profunda

El módulo HTTPoison proporciona una gran cantidad de opciones para personalizar nuestras solicitudes HTTP. Podemos especificar diferentes métodos (GET, POST, PUT, etc.), encabezados personalizados, autenticación y más. También podemos manejar errores y redireccionamientos de manera eficiente utilizando las funciones disponibles en este módulo. 

Además, Elixir también ofrece otros módulos para trabajar con HTTP, como Hackney y Tesla. Cada uno tiene sus propias características y ventajas, por lo que vale la pena explorarlos y elegir el más adecuado para nuestras necesidades.

## Ver también

- [Documentación de HTTPoison](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Guía de HTTP en Elixir](https://elixirschool.com/es/lessons/specifics/http/)
- [Página oficial de Elixir](https://elixir-lang.org/)