---
title:                "Last ned en nettstedsside"
html_title:           "Elixir: Last ned en nettstedsside"
simple_title:         "Last ned en nettstedsside"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor? 
Å laste ned en nettside betyr å hente informasjon fra en ekstern server og vise den på din egen enhet. Programmere gjør dette for å kunne vise informasjon fra forskjellige nettsteder på en enkel og effektiv måte.

## Slik gjør du: 
For å laste ned en nettside i Elixir bruker vi funksjonen `HTTPoison.get`. Denne funksjonen tar imot URL-en til nettsiden som et argument og returnerer et svarobjekt med all informasjonen fra nettstedet. Et eksempel på bruk av denne funksjonen kan se slik ut:

```elixir
url = "https://www.nrk.no"
response = HTTPoison.get(url)
```

Dette vil returnere et svarobjekt som inneholder informasjon om statuskoden, headerne og innholdet på nettsiden.

## Dykk dypere: 
Å laste ned nettsider har blitt en viktig del av programmering siden introduksjonen av internett. Det finnes flere måter å gjøre dette på, som å bruke biblioteker som `HTTPoison` eller å implementere en egen HTTP-klient. En alternativ måte er å bruke `HTTPoison.stream` som lar deg streame data fra nettsiden i stedet for å vente på at hele nettsiden blir lastet ned før du kan håndtere den.

## Se også: 
- [Elixir Documentation for HTTPoison](https://hexdocs.pm/httpoison/HTTPoison.html)
- [HTTP in Elixir: A deep dive](https://dev.to/boost/elixir-http-a-deep-dive-2knj)