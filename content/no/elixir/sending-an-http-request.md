---
title:                "Send en http forespørsel"
html_title:           "Elixir: Send en http forespørsel"
simple_title:         "Send en http forespørsel"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sende en HTTP-forespørsel er en viktig del av moderne webutvikling. Dette gjør det mulig for en applikasjon å kommunisere med andre applikasjoner, hente og sende data, og utføre handlinger på nettet.

## Slik Gjør Du

For å sende en HTTP-forespørsel i Elixir, kan du bruke `HTTPoison` biblioteket. Først må du legge det til i ditt `mix.exs` fil under `deps` seksjonen:

```Elixir 
def deps do
  [{:httpoison, "~> 1.5"}]
end
```

Deretter må du kjøre `mix deps.get` for å installere biblioteket.

For å sende en GET-forespørsel, kan du bruke `HTTPoison.get` funksjonen og angi URL-en som et argument:

```Elixir 
HTTPoison.get("https://example.com")
```

Dette vil returnere en tuple med to elementer - statuskode og responsdata. Du kan deretter hente dataene ved å bruke `HTTPoison.Response.body`:

```Elixir 
response = HTTPoison.get("https://example.com")
response_body = response.body
```

For å sende en POST-forespørsel med data, kan du bruke `HTTPoison.post` funksjonen og angi både URL-en og data som argumenter:

```Elixir 
HTTPoison.post("https://example.com", [body: "data"])
```

Du kan også inkludere HTTP-header og andre parametere i forespørselen ved å sende dem som en liste til `HTTPoison.get` eller `HTTPoison.post` funksjonene.

## Dypdykk

HTTP-forespørsler er bygget på toppen av TCP-protokollen og følger et bestemt format. De består av en forespørselslinje som inneholder metode, URL og HTTP-versjon, etterfulgt av en liste over header og et valgfritt kropp som inneholder forespørselens data.

Elixir's `HTTPoison` biblioteket forenkler prosessen med å sende HTTP-forespørsler ved å gi en abstraksjon over denne protokollen. Det håndterer også asynkron kommunikasjon og feilhåndtering.

## Se også

- [HTTPoison dokumentasjon](https://hexdocs.pm/httpoison/readme.html)
- [Elixir offisiell nettside](https://elixir-lang.org/)
- [TCP-protokollen forklart](https://www.cloudflare.com/learning/network-layer/what-is-tcp/)