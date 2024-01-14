---
title:    "Fish Shell: Att använda reguljära uttryck"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Att använda reguljära uttryck (regular expressions) i Fish Shell kan hjälpa till att effektivisera och göra kodningen mer lättläst. Reguljära uttryck är ett värdefullt verktyg för att söka, matcha och manipulera textsträngar.

## Hur man använder reguljära uttryck i Fish Shell

För att använda reguljära uttryck i Fish Shell, används kommandot `grep` eller `sed`. Båda dessa kommandon använder reguljära uttryck för att söka och manipulera text.

```Fish Shell
# Sök efter ett ord i en textfil
grep "hello" min_textfil.txt

# Sök efter ett ord och ersätt det med ett annat
sed -i "s/hello/world/g" min_textfil.txt
```

Kommandot `grep` kan också användas för att söka efter mönster i flera filer samtidigt.

```Fish Shell
# Sök efter "hello" i alla txt-filer i mappen
grep "hello" *.txt
```

Fish Shell har också inbyggda variabler som kan användas i reguljära uttryck. Till exempel kan variabeln `$fish_version` användas för att söka efter specifika versioner av Fish Shell i en textfil.

## Djupdykning i reguljära uttryck

Det finns många olika typer av reguljära uttryck och de kan vara komplexa att förstå. Det är viktigt att lära sig de olika metakaraktärerna och hur de används för att bygga upp ett reguljärt uttryck.

Några vanliga metakaraktärer inkluderar:

- `.` : Matchar vilken som helst enskild karaktär.
- `*` : Matchar noll eller fler förekomster av den föregående karaktären.
- `+` : Matchar en eller fler förekomster av den föregående karaktären.
- `?` : Matchar en eller ingen förekomst av den föregående karaktären.
- `^` : Matchar början av en sträng.
- `$` : Matchar slutet av en sträng.
- `\` : Används för att escapera metakaraktärer.

Det finns också möjlighet att använda olika flaggor med reguljära uttryck för att modifiera hur de söker efter mönster, till exempel att ignorera skiftlägeskänslighet eller söka över flera rader.

## Se även

- [Fish Shell dokumentation om reguljära uttryck](https://fishshell.com/docs/current/tutorial.html#tut_regex)
- [Reguljära uttryck Cheat Sheet](https://devhints.io/regex)