---
title:                "Analys av html"
html_title:           "Fish Shell: Analys av html"
simple_title:         "Analys av html"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

Vad och varför?

Att "parsa" HTML är att extrahera och organisera information från en HTML-kodad webbsida. Detta är användbart för programmerare eftersom det gör det möjligt att automatiskt samla data från webbsidor, till exempel för webbskrapning eller webbsidetestning.

Hur man gör det:

Fish Shell har ett inbyggt verktyg som heter "strings", som kan anvädas för att filtrera ut information från en HTML-kodad webbsida. Ett exempel kan se ut som följande:

```Fish Shell
strings http://www.example.com | grep "title"
```

Detta kommando kommer att söka igenom sidan för all text som innehåller ordet "title" och visa den i terminalfönstret. Det är ett enkelt sätt att snabbt få ut specifik information från en webbsida.

Djupdykning:

Att pars-koda HTML är en vanlig uppgift för webbutvecklare och kan också utföras med andra programmeringsspråk som Python eller Ruby. Andra alternativ till Fish Shell's "strings" inkluderar verktyg som "grep" eller "sed". Fish Shell's "strings" erbjuder en bekväm och lättanvändlig metod för att samla data från HTML-kodade webbsidor.

Se även:

- Fish Shell's officiella dokumentation för "strings" kommandot: https://fishshell.com/docs/current/cmds/strings.html 
- En introduktion till webbskrapning med Fish Shell: https://fishshell.com/blog/2020/01/10/web-scraping-with-fish.html