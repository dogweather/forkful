---
title:                "Descargando una página web"
html_title:           "Arduino: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Descargar una página web significa tomar los datos (usualmente HTML) de un servidor y guardarlos en una ubicación local. Los programadores hacen esto para trabajar con el contenido de la página sin necesidad de estar siempre conectados a internet.

## Cómo hacerlo:

En Elixir, podemos usar la librería HTTPoison para descargar una página web. Al utilizar la función `get`, enviamos una petición GET al URL proporcionado y recibimos una respuesta.

```elixir
defmodule MiModulo do
  def descargar_pagina(url) do
    case HTTPoison.get(url) do
      {:ok, respuesta} ->
        {:ok, respuesta.body}
      {:error, _} ->
        {:error, "No se pudo descargar la pagina"}
    end
  end
end

IO.inspect(MiModulo.descargar_pagina("http://example.com"))
```
Cuando ejecutas este código, debes ver el contenido de la página que especificaste en la consola.

## Profundizando

Cuando descargamos una página web, en realidad estamos interactuando con la parte el lado servidor de la web. Esta práctica se lleva a cabo desde los primeros días del internet, cuando las conexiones eran lentas y era más óptimo descargar el contenido para verlo localmente.

En cuanto a las alternativas, si estás trabajando con Elixir, `HTTPoison` es la opción más común y directa. Sin embargo, otras librerías como `Tesla` también son válidas y ofrecen diferentes ventajas, como una configuración y uso más flexible.

En cuanto a los detalles de implementación, `HTTPoison.get(url)` realiza una petición HTTP utilizando el método GET. El servidor responde enviando los datos solicitados, que `HTTPoison` luego guarda en el cuerpo de la respuesta (`response.body`).

## Ver También

Aquí hay algunos enlaces a recursos útiles:

1. Documentación de HTTPoison: https://hexdocs.pm/httpoison/readme.html
2. Guía de Tesla: https://hexdocs.pm/tesla/readme.html
3. Más detalles sobre las peticiones HTTP GET: https://developer.mozilla.org/es/docs/Web/HTTP/Methods/GET