---
title:    "Bash: Omvandla en sträng till gemener"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener är en vanlig uppgift inom Bash-programmering. Det kan vara användbart om man vill standardisera input från användare eller jämföra strängar på ett enhetligt sätt.

## Hur man gör det

Det finns flera olika sätt att konvertera en sträng till gemener i Bash. Ett sätt är att använda inbyggda funktionen "tr". Här är ett exempel:

```Bash
echo "HELLO WORLD" | tr '[:upper:]' '[:lower:]'
```

Detta kommer att ta strängen "HELLO WORLD" och konvertera den till "hello world". Det finns också möjlighet att använda inbyggda variabler som "tolower" eller "^^" för att konvertera en del av en sträng. Här är ett exempel på detta:

```Bash
text="HeLlO WoRlD"
echo "${text,,}"
```

Detta kommer att konvertera hela strängen till gemener, vilket ger outputen "hello world".

## Djupdykning

För att förstå mer om hur man konverterar strängar till gemener i Bash är det viktigt att ha en grundläggande förståelse för "tr" och dess användningsområden. Detta kommando kan inte bara användas för att konvertera mellan gemener och versaler, utan också för att ändra tecken eller ta bort tecken helt. Det är också värt att notera att "tolower" och "^^" fungerar endast med bash version 4 eller senare.

## Se även

- [Guide till "tr" kommandot](https://www.computerhope.com/unix/utr.htm)
- [Komplett lista över inbyggda bash kommandon](https://git-scm.com/book/en/v2/Git-Basics-Git-Aliases)
- [Mer information om inbyggda bash variabler](https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html)