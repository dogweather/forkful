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

## Por qué
Si estás acostumbrado a navegar por Internet, es probable que hayas descargado una página web antes. Pero, ¿alguna vez te has preguntado cómo funciona este proceso en términos de programación? En este artículo, te mostraremos cómo puedes usar Elixir para descargar una página web y obtener su contenido.

## Cómo hacerlo
Primero, necesitarás tener Elixir instalado en tu computadora. Puedes consultar la documentación oficial de Elixir para obtener instrucciones específicas en función de tu sistema operativo.

Una vez que tengas Elixir listo, puedes seguir estos pasos para descargar una página web:

1. Primero, importamos el módulo HTTPoison, que nos proporcionará funciones útiles para realizar solicitudes HTTP.
```Elixir
import HTTPoison
```

2. A continuación, usaremos la función `get/2` para realizar una solicitud GET a la URL que queremos descargar. Por ejemplo, si queremos descargar la página de inicio de Google, podemos usar:
```Elixir
response = get("https://www.google.com")
```

3. Ahora podemos obtener el contenido de la página en formato de cadena mediante la función `body/1`:
```Elixir
body = response |> body()
```

4. Finalmente, podemos imprimir el contenido en la consola o realizar cualquier otra operación con él:
```Elixir
IO.puts body
```

¡Y eso es todo! Con solo unos pocos pasos, podemos descargar y obtener el contenido de cualquier página web.

## Profundizando
Ahora que sabes cómo descargar una página web en Elixir, puedes explorar otras funciones del módulo HTTPoison, como la posibilidad de realizar solicitudes POST, configurar encabezados personalizados y mucho más. También puedes combinar el acceso a la web con otros módulos de Elixir, como el módulo `File` para guardar el contenido descargado en un archivo.

Recuerda que, aunque es muy útil, es importante usar esta funcionalidad de manera responsable y no abusar de ella. Siempre obtén el permiso adecuado antes de descargar el contenido de una página web.

## Ver también
- Documentación de Elixir: https://elixir-lang.org/docs.html
- Documentación de HTTPoison: https://hexdocs.pm/httpoison/HTTPoison.html
- Código fuente de este artículo: https://github.com/user/repo (donde "user" es tu nombre de usuario y "repo" es el nombre de tu repositorio en GitHub)