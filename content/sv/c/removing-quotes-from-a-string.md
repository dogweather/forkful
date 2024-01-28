---
title:                "Ta bort citattecken från en sträng"
date:                  2024-01-26T03:38:00.330379-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ta bort citattecken från en sträng"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att ta bort citattecken från en sträng innebär att man tar bort alla citattecken—vare sig det är enkla ('') eller dubbla ("")—som är en del av strängens innehåll. Programmerare gör detta för att sanera input, förbereda data för vidare bearbetning, eller för att undvika syntaxfel när de hanterar filvägar och kommandon i språk som använder citattecken för att avgränsa strängar.

## Hur man gör:

Här är en C-funktion som skrubbar bort de irriterande citattecknen ur dina strängar:

```c
#include <stdio.h>
#include <string.h>

void remove_quotes(char *str) {
    char *p_read = str, *p_write = str;
    while (*p_read) {
        if (*p_read != '"' && *p_read != '\'') {
            *p_write++ = *p_read;
        }
        p_read++;
    }
    *p_write = '\0';
}

int main() {
    char str[] = "He said, \"Hello, 'world'!\"";
    printf("Original: %s\n", str);
    remove_quotes(str);
    printf("Sanerad: %s\n", str);
    return 0;
}
```

Exempel på utdata:

```
Original: He said, "Hello, 'world'!"
Sanerad: He said, Hello, world!
```

## Fördjupning

Att ta bort citattecken från en sträng har varit en uppgift sedan programmeringens gryning, där datahygien var och fortfarande är nyckeln till att undvika fel (som SQL-injektionsattacker) eller för att säkerställa att en sträng säkert kan överföras till system som kanske förväxlar ett citattecken med ett kontrolltecken.

Historiskt sett hanterar olika språk denna uppgift på olika sätt—vissa har inbyggda funktioner (som `strip` i Python), medan andra, som C, kräver manuell implementering på grund av dess fokus på att ge utvecklare kontroll på lägre nivå.

Alternativ inkluderar att använda biblioteksfunktioner som `strpbrk` för att hitta citattecken eller att använda reguljära uttryck (med bibliotek som PCRE) för mer komplexa mönster, även om detta kan vara överdrivet för att bara ta bort citattecken.

Implementeringen ovan skannar helt enkelt igenom varje tecken i strängen, kopierar endast icke-citattecken till skrivpekarplatsen. Detta är effektivt eftersom det görs på plats utan att behöva extra minne för resultatsträngen.

## Se också

- [C Standardbiblioteksfunktioner](http://www.cplusplus.com/reference/clibrary/)
- [PCRE - Perl Compatible Regular Expressions](https://www.pcre.org/)
- [Förstå pekare i C](https://www.learn-c.org/en/Pointers)
