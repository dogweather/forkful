---
title:                "Elixir: Å laste ned en nettside"
simple_title:         "Å laste ned en nettside"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Hvorfor

Tenk deg at du ønsker å hente informasjon fra en nettside, kanskje for å bygge en app eller automatisere noe. Ved hjelp av Elixir kan du gjøre dette på en enkel og effektiv måte. I denne blogginnlegget vil vi diskutere hvordan man kan laste ned en nettside ved hjelp av Elixir.

# Hvordan

## URL bibliotek

Først må vi importere URL biblioteket som gjør det mulig å hente data fra en nettside. Dette kan gjøres ved å legge til følgende linje i din Elixir-fil:

```Elixir
iex> {:ok, url} = HTTPoison.get("https://www.example.com")
```

Her bruker vi `HTTPoison` biblioteket som lar oss gjøre http-forespørsler, og vi spesifiserer nettsiden vi ønsker å laste ned. Denne metoden returnerer `{:ok, url}` som betyr at forespørselen var vellykket, og gir oss en `url` variabel som inneholder dataen fra nettsiden.

## Hente data fra nettsiden

For å få tilgang til dataen fra nettsiden kan vi bruke `url` variabelen og bruke funksjonen `body` for å hente ut innholdet fra nettsiden. Dette kan gjøres ved å skrive følgende kode:

```Elixir
iex> data = url.body
```

Dataen som er hentet ut vil være i `UTF-8` format, slik at du kan enkelt behandle og manipulere den videre med Elixir funksjoner.

## Konvertere til tekst

For å få dataen i ren tekst-format kan vi bruke `HTTPoison` funksjonen `body_to_string`. Dette vil konvertere dataen til vanlig tekst og gjøre den enklere å håndtere. Koden for dette ser slik ut:

```Elixir
iex> text = url.body_to_string
```

Dette vil gi oss `text` variabelen som inneholder innholdet fra nettsiden i tekst-format.

# Dykk ned

Det er verdt å merke seg at `HTTPoison` biblioteket har mange andre funksjoner og muligheter som kan utforskes for å få enda mer kontroll over http-forespørslene. Du kan lese mer om dette i dokumentasjonen for biblioteket.

# Se også

Her er noen nyttige ressurser for å lære mer om hvordan man kan laste ned en nettside ved hjelp av Elixir:

- [Elixir Docs for HTTPoison](https://hexdocs.pm/httpoison/)
- [Elixir School - HTTP Library](https://elixirschool.com/en/lessons/specifics/http-libraries/)
- [How to Make a HTTP Request in Elixir](https://medium.com/swlh/how-to-make-a-http-request-in-elixir-d6d1fa61faee)