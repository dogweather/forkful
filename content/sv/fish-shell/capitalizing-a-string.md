---
title:                "Versalisering av en sträng"
html_title:           "Fish Shell: Versalisering av en sträng"
simple_title:         "Versalisering av en sträng"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför? 
Kapitalisering av en sträng är processen att göra den första bokstaven till en stor bokstav och resten av bokstäverna till små bokstäver. Programmare gör detta för att göra utdata mer lättläslig och enhetlig med kodningsstandarder.

## Hur man:
Fish Shell erbjuder flera sätt att kapitalisera en sträng. Här är några exempel: 

```Fish Shell
# använd kommandot string capitalize för att göra den första bokstaven stor
echo "programmering är roligt" | string capitalize
==> Programmering är roligt

# använd set-funktionen för att ändra storleken på alla bokstäver i en sträng 
set str = "FISH SHELL"
echo $str
==> FISH SHELL
set str (string tolower $str)
echo $str
==> fish shell
set str (string toupper $str)
echo $str
==> FISH SHELL

# använd sed för att ersätta den första bokstaven med en stor bokstav 
echo "hello world" | sed 's/^./\U&/'
==> Hello world
```

## Djupdykning:
Kapitalisering av en sträng har funnits sedan de tidigaste programmeringsspråken och används ofta för att bryta ner ord i mer lättläsliga delar. Det finns också flera olika sätt att göra detta, som inkluderar kommandon som ```tr```, ```sed``` och ```awk```. Fish Shell erbjuder dock en snabb och enkel metod för att kapitalisera strängar med hjälp av inbyggda funktioner.

## Se också:
- Fish Shell dokumentation för string capitalize kommandot (https://fishshell.com/docs/current/cmds/string.html#capitalize)
- Wikipedia artikeln om strängmanipulation (https://sv.wikipedia.org/wiki/Str%C3%A4ngmanipulation)