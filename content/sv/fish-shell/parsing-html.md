---
title:                "Analysera html"
html_title:           "Arduino: Analysera html"
simple_title:         "Analysera html"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att parsa HTML innebär att tolka och förstå HTML-kod för att utvinna specifik data eller information. Programmerare gör detta för att filtrera ut behövda data från webbsidor.

## Hur gör man:

Här är ett exempel på hur du extraherar alla länkar från en webbsida med hjälp av Fish Shell.

```fish
set webbplats "https://example.com"
set html (curl -s $webbplats)
echo $html | pup 'a attr{href}'
```

Detta kommer att ge en lista med alla href-attribut från ankare (<a>) element på din sida. 

## Fördjupning:

Parsing av HTML har pågått sedan HTML först skapades, och det finns otaliga metoder och alternativ tillgängliga. Trots detta är Fish Shell på uppgång för dess enkelhet och kraft.

Ett alternativ till Fish Shell är Python med Beautiful Soup eller Scrapy, vilka är mycket kraftfulla men kan vara klumpigare att implementera. 

Fish Shell, å andra sidan, har den fördelen att det är enkelt att bädda in i ditt befintliga system och kan användas direkt från terminalen.

Obs: Kom ihåg att följa reglerna för webbskrapande och att respektera robot.txt-filen på varje webbsida du interagerar med.

## Se Vidare:

- [Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [Pup HTML parser](https://github.com/ericchiang/pup)
- [HTML Standard](https://html.spec.whatwg.org/)
- [Webb skrapningsetik](https://towardsdatascience.com/ethics-in-web-scraping-b96b18136f01)