---
aliases:
- /es/elixir/sending-an-http-request/
date: 2024-01-20 17:59:21.389005-07:00
description: "Enviar una solicitud HTTP es el proceso de pedirle al servidor que responda\
  \ con datos o recursos. Los programadores lo hacemos para interactuar con APIs,\u2026"
lastmod: 2024-02-18 23:09:09.645093
model: gpt-4-1106-preview
summary: "Enviar una solicitud HTTP es el proceso de pedirle al servidor que responda\
  \ con datos o recursos. Los programadores lo hacemos para interactuar con APIs,\u2026"
title: Enviando una solicitud http
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Enviar una solicitud HTTP es el proceso de pedirle al servidor que responda con datos o recursos. Los programadores lo hacemos para interactuar con APIs, descargar archivos o simplemente para recibir información al desarrollar aplicaciones web o móviles.

## Cómo hacerlo:

Elixir tiene una biblioteca llamada HTTPoison que nos facilita enviar solicitudes HTTP. Aquí tienes un ejemplo de cómo hacer una petición GET:

```elixir
# Asegúrate de tener HTTPoison agregado a tus dependencias en mix.exs
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

# Ejemplo de cómo hacer una solicitud GET
def fetch_data(url) do
  case HTTPoison.get(url) do
    {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
      {:ok, body}
    {:ok, %HTTPoison.Response{status_code: status_code}} ->
      {:error, "Algo salió mal. Status: #{status_code}"}
    {:error, %HTTPoison.Error{reason: reason}} ->
      {:error, reason}
  end
end

# Uso de la función fetch_data
{:ok, body} = fetch_data("https://jsonplaceholder.typicode.com/posts/1")
IO.puts(body)
```

Este código muestra una solicitud GET a un API de prueba e imprime el resultado. Recuerda gestionar tus respuestas y errores de forma adecuada en tu aplicación.

## Detalles:

Enviar solicitudes HTTP es esencial desde el nacimiento de la web. Empezando con librerías básicas en otros lenguajes, Elixir proporciona una manera más moderna y conveniente con HTTPoison, que se basa en hackney, un cliente HTTP en Erlang.

Una alternativa a HTTPoison es la biblioteca `Tesla`, que viene con middleware y permite mayor flexibilidad. Otro enfoque es usar OTP y GenServer para manejar solicitudes simultáneamente e integrarlas en el sistema de supervisión.

En cuanto a la implementación, elegir entre sincrónico o asíncrono depende del caso de uso. Si necesitas realizar muchas solicitudes a una API externa, considera respuestas asíncronas para no bloquear el proceso.

## Ver También:

- [HTTPoison GitHub](https://github.com/edgurgel/httpoison)
- [Tesla GitHub](https://github.com/teamon/tesla)
- [Erlang’s HTTP client :hackney](https://github.com/benoitc/hackney)
