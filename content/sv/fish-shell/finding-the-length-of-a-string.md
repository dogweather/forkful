---
title:                "Hitta längden på en sträng"
html_title:           "Arduino: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Hitta längden på en sträng avser operationen att räkna antalet tecken i en specifik sträng. Programmerare gör detta när de behöver behandla data beroende på dess storlek, till exempel att trimma eller utöka text.

## Så här gör du:

Fish Shell gör det lätt för oss att hitta längden på en sträng med hjälp av strlength-funktionen. Låt oss titta på ett exempel.

```
Fish Shell
$ set sträng "Hej, värld!"
$ echo (string length -q $sträng)
```
Exemplet ovan sätter strängen till "Hej, värld!" och visar sedan längden på strängen, som är 12.

## Fördjupning

Historiskt sett har olika programmeringsspråk tillhandahållit olika metoder för att hitta längden på en sträng. I Fish Shell introducerades string length-funktionen för att förenkla den här uppgiften.

Ett alternativ till string length i Fish Shell är att använda wc -m funktionen. Dock kan detta returnera olika resultat om strängen innehåller speciella tecken.

När du hittar längden på en sträng är det viktigt att förstå att Fish Shell faktiskt räknar antalet tecken, inte bytes. Så för strängar med multi-byte karaktärer kan detta vara särskilt användbart.

## Se också

För mer information om strängmanipulation i Fish Shell, se följande länkar:
1. [Fish Shell: string length](https://fishshell.com/docs/current/cmds/string-length.html)
2. [Fish Shell: wc -m](https://fishshell.com/docs/current/cmds/wc.html)
3. [Fish Shell: set](https://fishshell.com/docs/current/cmds/set.html)