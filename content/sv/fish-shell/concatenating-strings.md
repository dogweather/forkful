---
title:                "Sammanslående av strängar"
html_title:           "Fish Shell: Sammanslående av strängar"
simple_title:         "Sammanslående av strängar"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Förmodligen har du stött på uppgiften att lägga samman flera textsträngar till en enda sträng. I detta fall kan du använda dig av Fish Shell för att effektivt utföra operationen och därmed spara tid och undvika onödigt komplex kod.

## Hur man gör

För att sammanfoga flera textsträngar i Fish Shell behöver du använda kommandot "string join", med syntaxen `string join STRÄNG1 STRÄNG2`. Detta sammanfogar `STRÄNG1` och `STRÄNG2` till en enda sträng.

```Fish Shell
string join "Hej" "världen"  # Ger output "Hej världen"
```

Om du vill sammanfoga flera strängar i en variabel kan du använda följande syntax:

```Fish Shell
set mitt_namn "Lisa"
string join "Mitt namn är" $mitt_namn ". Jag gillar att programmera." # Ger output "Mitt namn är Lisa. Jag gillar att programmera."
```

## Djupdykning

I Fish Shell finns det flera andra kommandon som hjälper till att manipulera och sammanfoga strängar. Till exempel kan du använda kommandot "string split" för att dela upp en sträng baserat på ett visst tecken.

```Fish Shell
set mejladress "lisa@example.com"
string split "@" $mejladress  # Ger output "lisa" "example.com"
```

Du kan också kombinera flera strängar med kommandot "string append". Detta lägger till en sträng i slutet av en befintlig sträng.

```Fish Shell
set favorit_frukt "äpplen"
string append "Jag älskar att äta " $favorit_frukt " på sommaren." # Ger output "Jag älskar att äta äpplen på sommaren."
```

## Se också

- Fish Shell officiell dokumentation: https://fishshell.com/docs/current/index.html
- Fish Shell tutorial: https://fishshell.com/docs/current/tutorial.html