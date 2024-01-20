---
title:                "Analysering av html"
html_title:           "C#: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å parse HTML handler om å tolke og bryte ned HTML-kode til forståelig datastruktur. Dette gjør programmerere for å hente, manipulere og analysere data fra websider på en mer effektiv måte.

## Slik gjør du det:

I TypeScript kan vi bruke biblioteket `parse5` for å parse HTML. Her er et grunnleggende eksempel:

```TypeScript
import * as parse5 from 'parse5';

const html = `<body><p>Hello, verden!</p></body>`;
const document = parse5.parse(html);

console.log(document.childNodes[0].nodeName); // utskrift: "#document"
```

Kjøring av dette programmet vil parse `<body><p>Hello, verden!</p></body>`, og deretter skrive ut nodenavnet til den første child noda. 

## Dyp dykk:

Parsing av HTML har vært relevant siden webens oppstart, da det gir programmører muligheten til å interagere med internett på en strukturert måte. Tidligere metoder for parsing HTML har blant annet omfattet regulære uttrykk, men dette kan bli raskt komplisert med komplekse HTML-strukturer. TypeScript gir en mer moderne og robust løsning.

Alternativer til `parse5` inkluderer biblioteker som `htmlparser2` eller `cheerio`, som har forskjellige funksjoner og prestasjonsnivåer basert på dine behov. 

Når vi snakker om implementeringsdetaljer, er det viktig å merke seg at parsing ikke nødvendigvis vil gi akkurat samme resultater på tvers av forskjellige plattformer eller nettlesere. Dette er grunn til `quirks mode` og variasjoner i hvordan ulike nettlesere tolker HTML.

## Se også:



3. [Cheerio.js API Dokumentasjon](https://cheerio.js.org/)