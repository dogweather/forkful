---
title:                "Å sende en HTTP-forespørsel"
date:                  2024-01-20T17:59:33.227832-07:00
model:                 gpt-4-1106-preview
simple_title:         "Å sende en HTTP-forespørsel"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP-forespørsel handler om å be internett-tjenester om informasjon eller utføre en handling. Programmerere gjør dette for å samhandle med web-APIer, hente data, sende skjemaer, og mer.

## Slik gjør du:
For å sende en HTTP-forespørsel i Elixir, kan du bruke det innebygde `HTTPPoison`-biblioteket. Her er et eksempel:

```elixir
# Legg til HTTPPoison til prosjektets mix.exs
defp deps do
  [{:httpoison, "~> 1.8"}]
end

# Kjør `mix deps.get` i terminalen for å hente ned avhengigheten

# Slik sender du en GET-forespørsel
defmodule MyHTTPClient do
  def fetch(url) do
    HTTPoison.get(url)
  end
end

# Eksempel på bruk
{:ok, response} = MyHTTPClient.fetch("http://httpbin.org/get")
IO.inspect(response.body)
```

Når du kjører koden over, vil du se svaret fra `httpbin.org`.

## Dykk Dypere
HTTP-forespørsler i Elixir ble hovedsakelig foretatt med `HTTPoison`-biblioteket, som er en innpakning rundt `hackney` Erlang-biblioteket. Dette biblioteket har vært en støttepilar, men det er også alternative biblioteker som `Tesla` eller lavnivå-tilnærminger som bruker `:httpc` som kommer med Erlang/OTP.

`HTTPoison` gjør async-forespørsler og støtter mange funksjoner, inkludert timeouts og grunnleggende autentisering. For et dypere dykk, utforsk hvordan OTP (Open Telecom Platform) påvirker håndtering av HTTP-forbindelser og parallellisme, essensielt for å forstå ytelse og feilhåndtering.

## Se Også
- [HTTPoison GitHub repo](https://github.com/edgurgel/httpoison)
- [Tesla GitHub repo](https://github.com/teamon/tesla)
- Elixir's offisielle dokumentasjon for [HTTP-klienter](https://elixir-lang.org/docs.html)
- [Erlang :httpc dokumentasjon](http://erlang.org/doc/man/httpc.html)
