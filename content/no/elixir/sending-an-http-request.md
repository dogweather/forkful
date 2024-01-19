---
title:                "Å sende en http-forespørsel"
html_title:           "C++: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Å sende en HTTP-forespørsel i Elixir 

Elixir er et flott verktøy for å håndtere nettverksforespørsler. Her er en hurtig veiledning for HTTP-forespørsler. 

## Hva & Hvorfor?

HTTP-forespørsler er forespørsler en klient sender til en server for ett av fire formål: hente, lage, oppdatere eller slette data. Programmerere gjør dette for å få tilgang til data i applikasjoner og tjenester utenfra. 

## Hvordan:

Her er hvordan du sender en GET-forespørsel for å hente data ved hjelp av Elixir. Vi skal bruke biblioteket HTTPoison. 

```elixir
defmodule MyModule do
  require HTTPoison
  def request do
    {:ok, response} = HTTPoison.get("https://eksempel.com/navn")
    IO.puts response.body
  end
end
```

Når du kjører denne koden vil responsen fra "https://eksempel.com/navn" bli skrevet ut i terminalen.

## Dyp Dykk

1. Historisk Kontekst: Elixir bruker Erlang's HTTPc-modul for å sende HTTP-forespørsler. Erlang ble utviklet på Ericson, og bruker en svært pålitelig, høyt konkurransedyktig "aktørmodell". 
   
2. Alternativer: Andre alternativer til HTTPoison inkluderer Tesla og HTTPotion som er ganske populære. Disse gir mange av de samme funksjonene og det endelige valget kommer nok ned til personlig preferanse.

3. Implementasjonsdetaljer: Elixir tilbyr flere metoder for å sende HTTP-forespørsler. Du kan sende synkrone (blokkerende) eller asynkrone (ikke-blokkerende) forespørsler.
   
## Se også

- HTTPoison dokumentasjon: https://hexdocs.pm/httpoison/readme.html
- Tesla dokumentasjon: https://hexdocs.pm/tesla/readme.html
- HTTPotion dokumentasjon: https://hexdocs.pm/httpotion/readme.html
- Erlang dokumentasjon: https://erlang.org/doc/apps/inets/http_client.html