---
title:                "Radera tecken som matchar ett mönster"
html_title:           "Bash: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att ta bort tecken som matchar ett mönster är en vanlig uppgift för programmerare. Det är ett sätt att filtrera och manipulera data eller text på ett effektivt sätt. Genom att ta bort onödiga tecken kan man skapa en renare och mer läsbar kod.

## Hur man gör:

Det finns flera sätt att ta bort tecken som matchar ett mönster i Bash. Ett sätt är att använda kommandot `sed` tillsammans med flaggan `-i` för att ändra filen direkt. Exempelvis kan man ta bort alla förekomster av "a" från en fil med kommandot:

```Bash
sed -i 's/a//g' file.txt
```

Detta kommer att ta bort alla "a" från filen `file.txt` och spara ändringarna i samma fil. Man kan också använda flaggan `-r` för att matcha mönster med reguljära uttryck. Till exempel kan man ta bort alla siffror från en fil med:

```Bash
sed -ir 's/[0-9]//g' file.txt
```

Ett annat sätt att ta bort tecken är genom att använda `tr` kommandot. Detta kommando byter ut tecken eller tar bort dem helt baserat på det angivna mönstret. Exempelvis kan man ta bort alla mellanslag från en fil med:

```Bash
tr -d ' ' < file.txt > new_file.txt
```

Detta kommer att skapa en ny fil `new_file.txt` utan mellanslag.

## Djupdykning:

Historiskt sett har den vanligaste metoden för att ta bort tecken som matchar ett mönster varit med hjälp av `sed`. Men numera finns det flera alternativ som till exempel `awk`, `grep` och `tr`. Alla dessa kommandon har sina egna styrkor och kan användas för mer komplexa uppgifter än bara att ta bort tecken.

I Bash är en teckenmatchning ett uttryck som matchar en eller flera tecken i en sträng. Detta uttryck kan bestå av bokstäver, siffror eller tecken. Man kan också använda metatecken som wildcard-tecknet `*` för att matcha en mängd olika tecken. Det finns många olika sätt att uttrycka ett mönster och det är viktigt att förstå detta för att kunna använda de olika kommandona effektivt.

## Se även:

- Bash dokumentation: https://www.gnu.org/software/bash/manual/bash.html
- Sed dokumentation: https://www.gnu.org/software/sed/manual/sed.html
- Tr dokumentation: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html