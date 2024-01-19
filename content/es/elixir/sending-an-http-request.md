---
title:                "Enviando una solicitud http"
html_title:           "Bash: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Enviar una solicitud HTTP en la programación es el acto de solicitar datos a un servidor utilizando el protocolo HTTP. Los programadores hacen esto para interactuar con servicios web y obtener o enviar datos.

## Cómo hacerlo:

Aquí hay un ejemplo sencillo de cómo enviar una solicitud GET utilizando `HTTPoison`, una popular biblioteca HTTP para Elixir.

Para instalar HTTPoison, añade `{:httpoison, "~> 1.8"}` a tus dependencias en `mix.exs`.

```Elixir
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

Y ejecuta `mix deps.get` para descargar la dependencia.

Una vez instalada, aquí tienes un ejemplo simple:

```Elixir
defmodule MyModule do
  def get_request do
    url = "http://mi-api.com/datos"
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        IO.puts "Datos obtenidos correctamente: #{body}"
      {:ok, %HTTPoison.Response{status_code: status_code}} ->
        IO.puts "Error obteniendo los datos: #{status_code}"
      {:error, %HTTPoison.Error{reason: reason}} ->
        IO.puts "Error: #{reason}"
    end
  end
end
```

## En Profundidad:

Las solicitudes HTTP son esenciales en el desarrollo web desde el nacimiento de la web. Sin ellas, la web como la conocemos no existiría.

Alternativas a HTTPoison incluyen `Tesla` y `Mint`. Tesla tiene una interfaz similar, mientras que Mint ofrece un control más bajo a nivel de TCP. 

Cuando envías una solicitud HTTP con HTTPoison, en realidad, estás utilizando la biblioteca `hackney` de Erlang bajo el capó. `hackney` es un cliente HTTP simple y eficiente, y HTTPoison es solo un delgado envoltorio Elixir alrededor de él. 

## Ver También:

- Documentación de HTTPoison: https://hexdocs.pm/httpoison/readme.html
- Tesla, una alternativa: https://github.com/teamon/tesla
- Mint, para un control más bajo sobre las solicitudes HTTP: https://github.com/elixir-mint/mint