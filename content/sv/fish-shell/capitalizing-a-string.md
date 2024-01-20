---
title:                "Gör om en sträng till versaler"
html_title:           "Fish Shell: Gör om en sträng till versaler"
simple_title:         "Gör om en sträng till versaler"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Kapitalisering av en sträng innebär att göra den första bokstaven i varje ord stort. Programmerare gör detta för att förbättra läsbarheten och presentationen av text i program.

## Hur man gör:

Här är ett exempel om hur man kapitaliserar en sträng i Fish Shell.

```Fish Shell
function to_uppercase --description "Konverterar första bokstaven i varje ord till stor"
    set -l str $argv
    echo (string upper $str)
end
```

Låt oss pröva det.

```Fish Shell
> to_uppercase "detta är en teststräng"
DETTA ÄR EN TESTSTRÄNG
```

## Fördjupning:

Historiskt sett har metoder för strängkapitalisering sitt ursprung i behovet av att framhäva titlar och avsnittsöverskrifter inom textbehandling. 

I Fish Shell 3.0 inleddes stöd för inbyggda strängfunktioner som 'string upper', vilket förenklade strängmanipulering.

Alternativt kan du använda awk eller sed för att kapitalisera en sträng, men detta kan bli mer komplicerat och mindre läsbart, särskilt för större scripts.

För implementering, se till att använda (string upper) operatören som förvandlar alla små bokstäver i ett strängargument till stora bokstäver. Om ingen string ges, läser och bearbetar den från stdin.

## Se även:

- [Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [Tutorial för Fish Shell Scripting](http://fishshell.com/docs/current/tutorial.html)
- [Fish Shell GitHub Repo](https://github.com/fish-shell/fish-shell)