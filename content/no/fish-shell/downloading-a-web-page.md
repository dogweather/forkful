---
title:                "Laste ned en nettside"
html_title:           "Fish Shell: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å laste ned en nettside betyr å hente innholdet på den, slik at du kan vise den på datamaskinen din. Programmere gjør dette for å automatisere prosesser og hente nyttig informasjon fra nettet, som for eksempel å hente ut data for å analysere eller bruke i sine egne programmer.

## Hvordan:

I Fish Shell kan du bruke kommandoen `curl` for å laste ned en nettside. Her er et eksempel på hvordan du laster ned Google sin hjemmeside og lagrer den i en fil kalt "google.html":

```
fish shell> curl "https://www.google.com" > google.html
```

Dette vil laste ned innholdet på Google sin nettside og lagre det i en fil. Du kan deretter åpne denne filen i en nettleser for å se resultatet.

## Dykke Dypere:

Å laste ned en nettside er ikke noe nytt, og det finnes flere alternativer til `curl` kommandoen som kan brukes i stedet. Noen programmerere foretrekker å bruke mer avanserte verktøy som for eksempel Python-biblioteker som Requests eller BeautifulSoup.

Implementeringen av `curl` i Fish Shell er basert på et eksternt verktøy som er installert på datamaskinen din, så det kan være lurt å sjekke om det er installert på forhånd.

## Se Også:

- [Fish Shell Docs: Curl](https://fishshell.com/docs/current/cmds/curl.html)
- [Requests Python-bibliotek](https://requests.readthedocs.io/en/master/)
- [BeautifulSoup Python-bibliotek](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)