---
title:    "Bash: Extrahera delsträngar"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Att extrahera substränger kan vara en mycket användbar färdighet inom Bash-programmering. Genom att använda sig av denna teknik kan man utvinna viktig information från en större sträng och använda den för att utföra olika åtgärder. Det är också ett vanligt förekommande problem som man stöter på vid skriptutveckling, så det är definitivt en bra kunskap att ha under bältet.

## Så här gör du

För att extrahera en substräng från en större sträng kan man använda sig av "substring expansion" i Bash. Syntaxen för detta är ${string:start:length}, där "string" är den ursprungliga strängen, "start" är det index där man vill påbörja substrängen och "length" är antalet tecken som man vill extrahera. Här är ett exempel:

```Bash
original_string="Hej! Jag heter Johan."
# Extrahera substrängen "Jag heter"
substring="${original_string:5:9}"
echo $substring # Output: Jag heter
```

Man kan också använda negativa index för att räkna bakifrån i strängen. Om man till exempel vill extrahera de sex sista tecknen i en sträng kan man använda ${string: -6}.

```Bash
original_string="Detta är en sträng."
# Extrahera de sex sista tecknen - "sträng."
substring="${original_string: -6}"
echo $substring # Output: sträng.
```

Det finns också andra sätt att extrahera substränger, som att använda grep-kommandot eller sed-kommandot, men dessa kräver mer avancerade kunskaper inom Bash.

## Fördjupning

När man extraherar substränger i Bash är det viktigt att förstå att teckenföljden börjar på index 0 istället för 1. Detta kan orsaka förvirring för nybörjare, men det är en viktig del av Bash-syntaxen som man bör vara medveten om.

Man kan också använda variabler för att definiera startindex och längd för en substräng, vilket kan göra koden mer flexibel och lättare att förstå. Det är också viktigt att komma ihåg att om man försöker extrahera en längre substräng än vad som finns tillgängligt i den ursprungliga strängen så kommer bara de tillgängliga tecknen att visas.

## Se även

- [Bash-dokumentationen om "substring expansion"](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [Grep-kommandot i Bash](https://www.gnu.org/software/grep/manual/grep.html)
- [Sed-kommandot i Bash](https://www.gnu.org/software/sed/manual/sed.html)