---
title:                "Analyse av HTML"
html_title:           "Lua: Analyse av HTML"
simple_title:         "Analyse av HTML"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing HTML betyr å eksperimentere med og manipulere HTML-kode for å få ønsket informasjon eller utseende på en nettside. Dette er en viktig oppgave for programmører for å kunne skreddersy og forbedre nettsider.

## Hvordan:
```Lua
local html = "<p>Hello, <span>world!</span></p>"
local parsed = string.gmatch(html, "<p>(.*)</p>")
print(parsed()) -- Output: Hello, <span>world!</span>
```

## Deep Dive:
Parsing HTML startet som en manuell prosess for å tolke og bygge nettsider på 1990-tallet. Men i dag kan programmører bruke automatiserte verktøy som "regular expressions" og HTML-parsere for å effektivt analysere og manipulere HTML-kode. Alternativer til Lua for parsing HTML inkluderer språk som Python og JavaScript. Implementering av HTML-parser er ofte basert på å lese og tolke HTML-tagene for å bygge et DOM (Document Object Model) tre som kan utføres videre.

## Se også:
[HTML-parsing libraries for Lua](https://rosettacode.org/wiki/Category:HTML-parsing)