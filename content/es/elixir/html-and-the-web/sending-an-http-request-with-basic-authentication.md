---
date: 2024-01-20 18:01:18.051876-07:00
description: "C\xF3mo Hacerlo: La autenticaci\xF3n b\xE1sica HTTP es un m\xE9todo\
  \ antiguo pero simple para controlar el acceso a recursos web. No es el m\xE1s seguro\
  \ porque las\u2026"
lastmod: '2024-04-05T21:54:00.056745-06:00'
model: gpt-4-1106-preview
summary: "La autenticaci\xF3n b\xE1sica HTTP es un m\xE9todo antiguo pero simple para\
  \ controlar el acceso a recursos web."
title: "Enviando una solicitud http con autenticaci\xF3n b\xE1sica"
weight: 45
---

## Cómo Hacerlo:
```elixir
# Primero, añade la dependencia :httpoison en mix.exs
defp deps do
  [{:httpoison, "~> 1.8"}]
end

# Luego, ejecuta `mix deps.get` para instalar la dependencia

# Después, aquí hay un ejemplo simple de una solicitud GET con autenticación básica usando HTTPoison:
defmodule HttpClient do
  def get_with_basic_auth(url, username, password) do
    auth = :base64.encode("#{username}:#{password}")
    headers = [{"Authorization", "Basic #{auth}"}]

    HTTPoison.get(url, headers)
  end
end

# Uso:
{:ok, response} = HttpClient.get_with_basic_auth("https://api.ejemplo.com/data", "usuario", "contraseña")

# Output:
# response.body tendrá el contenido de la respuesta
# response.status_code será el código de estado HTTP
```

## Inmersión Profunda:
La autenticación básica HTTP es un método antiguo pero simple para controlar el acceso a recursos web. No es el más seguro porque las credenciales van codificadas en base64, que es fácilmente decodificable. Es por eso que es fundamental usar HTTPS para encriptar la comunicación.

Alternativas incluyen OAuth, que es más complejo pero también más seguro. Aun así, la autenticidad básica puede ser útil para servicios internos o para pruebas rápidas.

En el caso de Elixir, HTTPoison se basa en Hackney, que maneja la conexión HTTP subyacente. Otras librerías como Tesla también podrían ser consideradas, pero HTTPoison es popular por su simplicidad y fluidez en Elixir.

## Ver También:
- [HTTPoison GitHub repository](https://github.com/edgurgel/httpoison)
- [Base64 encode/decode in Elixir](https://hexdocs.pm/elixir/Base.html#encode64/2)
- [Tesla, otra librería Elixir HTTP client](https://github.com/teamon/tesla)
