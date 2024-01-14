---
title:                "Elixir: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sende HTTP-forespørsler er en viktig del av å utvikle web-applikasjoner og APIer. Det lar oss kommunisere med ulike tjenester og hente data som vi trenger for å bygge dynamiske og interaktive applikasjoner. I Elixir, kan vi enkelt sende HTTP-forespørsler ved hjelp av det innebygde `HTTPoison` biblioteket.

## Hvordan

For å sende en HTTP-forespørsel i Elixir, må vi først installere og importere `HTTPoison` biblioteket i prosjektet vårt.

```
mix deps.get
```
```
iex> Httpoison.start
```

Deretter kan vi bruke funksjonen `get` til å sende en GET-forespørsel til en bestemt URL.

```
response = HTTPoison.get("http://www.example.com")
```

Vi kan også legge til parametere og headers til forespørselen vår.

```
response = HTTPoison.get(url, [body: "payload", headers: ["Authorization": "token"]])
```

Når vi har fått en respons, kan vi få tilgang til statuskoden, responsens kropp og headers.

```
HTTPoison.get(url) |> respstatus
```
```
HTTPoison.get(url) |> respbody
```
```
HTTPoison.get(url) |> respheaders
```

## Dypdykk

Bak kulissene bruker `HTTPoison` biblioteket Erlang's `HTTPc` modul for å håndtere HTTP-kommunikasjon. Dette gir oss en pålitelig og stabil måte å sende og motta HTTP-forespørsler på. Vi kan også konfigurere `HTTPoison` til å bruke en HTTP-proxy eller å håndtere feil på en spesiell måte.

## Se også

- Offisiell dokumentasjon for HTTPoison: https://hexdocs.pm/httpoison/
- En guide for å håndtere feilsituasjoner i HTTPoison: https://devato.com/post/http-error-handling-with-httpoison/
- En introduksjon til Elixir og HTTP: https://medium.com/@geo_bash/elixir-http-get-requests-cb3a3b1bbf1d