---
title:                "Bash: Radering av tecken som matchar ett mönster"
simple_title:         "Radering av tecken som matchar ett mönster"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort tecken som matchar ett mönster kan vara användbart i olika bash-script för att manipulera textsträngar eller skapa filtreringsfunktioner för data.

## Hur man gör

För att ta bort tecken som matchar ett mönster i Bash använder vi kommandot `sed` och dess sökfunktion. Här är en kodexempel på hur man tar bort alla siffror från en textsträng:

```
Bash
str="Det här är en123 text456."
echo "$str" | sed 's/[0-9]//g'
```

Koden ovan kommer att ta bort alla siffror från textsträngen och ge oss resultatet "Det här är en text".

## Djupdykning

För att förstå hur det här fungerar behöver vi först veta vad ett "mönster" är inom Bash. Det är ett uttryck som används för att matcha textsträngar med hjälp av wildcards, som `*` eller `?`. I koden ovan använde vi mönstret `[0-9]` för att matcha alla siffror. `s` är en del av sökfunktionen `sed` och indikerar att vi vill byta ut texten som matchar mönstret med en tom sträng. `g` gör att operationen utförs för alla matchningar istället för bara den första.

Det finns flera andra användbara kommandon inom Bash för att manipulera textsträngar, såsom `grep` och `awk`. Genom att lära sig dessa kan du utveckla kraftfulla script för att hantera textbaserade data.

## Se även

- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Bash Scripting Tutorial](https://ryanstutorials.net/bash-scripting-tutorial/)
- [The Art of Command Line](https://github.com/jlevy/the-art-of-command-line)