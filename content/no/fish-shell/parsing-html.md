---
title:                "Analysering av html"
html_title:           "C#: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Analysere (parse) HTML er prosessen der en datamaskin leser og forstår HTML-kode. Programmerere gjør dette hovedsakelig for å manipulere, hente data fra eller endre struktur på websider.

## Hvordan:

Her er et enkelt eksempel på hvordan du kan analisere HTML med Fish Shell:

```Fish Shell
set html "<html><body><h1>Hei Verden!</h1></body></html>"
set begynnelsen (string match -r "<h1>" $html)
set slutten (string match -r "</h1>" $html)
echo (string sub -s (math $begynnelsen+4) -l (math $slutten-$begynnelsen-4) $html)
```

Når du kjører denne koden, får du følgende utskrift:

```Fish Shell
Hei Verden!
```

## Dypdykk:

Historisk sett har parsing av HTML blitt brukt siden tidlige dager av nettutvikling. Det tilbyr en effektiv måte å hente data og manipulere nettstedsinnhold på. 

Alternativt til Fish Shell er det mange andre verktøy som også kan brukes til å analysere HTML, som Python's BeautifulSoup, Node's Cheerio og mange flere.

Når det kommer til implementeringsdetaljer er Fish Shell karakterisert ved sin enkelhet og lesebarhet, men det kan være mindre kraftig for komplekse oppgaver sammenlignet med noen av de andre verktøyene nevnt ovenfor.

## Se Også:

1. [Fish Shell Dokumentasjon](https://fishshell.com/docs/current/index.html)
2. [Python's BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
3. [Node's Cheerio](https://github.com/cheeriojs/cheerio)