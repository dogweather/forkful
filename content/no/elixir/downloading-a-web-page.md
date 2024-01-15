---
title:                "Last ned en nettside"
html_title:           "Elixir: Last ned en nettside"
simple_title:         "Last ned en nettside"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor

Å laste ned en nettside kan være en nyttig ferdighet å ha for å bygge webapplikasjoner, automatisere oppgaver eller som en del av et større prosjekt. Det kan også være nyttig for å få tilgang til informasjon på en nettside når du ikke har en pålitelig internettilkobling.

## Slik gjør du det

```Elixir
# Først må vi laste inn HTTP-biblioteket
require HTTPoison

# Deretter kan vi bruke get-funksjonen for å sende en forespørsel og få tilbake en respons
HTTPoison.get("https://www.example.com")

# Hvis vi ønsker å lagre responsen i en variabel, kan vi gjøre det ved å bruke pipelining
response = HTTPoison.get("https://www.example.com")
```

Dette vil gi oss en respons-oppføring med informasjon om statuskoder, kropp og headers. For å få tilgang til responskroppen, kan vi bruke:

```Elixir
response.body
```

Dette vil returnere en liste med byte-kode som vi kan konvertere til en streng for å få det endelige resultatet. For å lagre resultatet til en fil, kan vi bruke:

```Elixir
File.write!("nettsted.html", response.body)
```

Dette vil lagre nettsiden som en .html-fil i samme mappe som koden.

## Dypdykk

Nå som vi kan laste ned en nettside og lagre den som en fil, kan vi enkelt behandle og manipulere dataene etter behov. Dette kan være nyttig for å analysere nettinnhold, automatisk oppdatering av siden eller for å generere en samling av nettsider.

Det er også mulig å legge til tilleggsparametere i get-funksjonen for å inkludere autentisering, spesifisere HEADERS eller bruke proxy-servere.

## Se også

- [Offisiell Elixir Dokumentasjon (engelsk)](https://elixir-lang.org/docs.html)
- [HTTPoison GitHub Repository (engelsk)](https://github.com/edgurgel/httpoison)