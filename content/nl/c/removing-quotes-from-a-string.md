---
title:                "Quotes verwijderen uit een string"
date:                  2024-01-28T22:06:08.440498-07:00
model:                 gpt-4-0125-preview
simple_title:         "Quotes verwijderen uit een string"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/removing-quotes-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het verwijderen van aanhalingstekens uit een string betekent het wegstrippen van enige aanhalingstekens - hetzij enkele ('') of dubbele ("") - die deel uitmaken van de inhoud van de string. Programmeurs doen dit om invoer te saneren, gegevens voor te bereiden voor verdere verwerking, of om syntaxisfouten te vermijden bij het omgaan met bestandspaden en commando's in talen die aanhalingstekens gebruiken om strings af te bakenen.

## Hoe doe je dat:

Hier is een C-functie die die vervelende aanhalingstekens uit je strings zal schrobben:

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
    printf("Origineel: %s\n", str);
    remove_quotes(str);
    printf("Gezuiverd: %s\n", str);
    return 0;
}
```

Voorbeelduitvoer:

```
Origineel: He said, "Hello, 'world'!"
Gezuiverd: He said, Hello, world!
```

## Diepgaande duik

Het verwijderen van aanhalingstekens uit een string is een taak die sinds het begin van het programmeren bestaat, waar datahygiëne toen en nog steeds de sleutel is tot het vermijden van fouten (zoals SQL-injectie-aanvallen) of om ervoor te zorgen dat een string veilig doorgegeven kan worden aan systemen die een aanhalingsteken mogelijk als een besturingskarakter kunnen zien.

Historisch gezien handelen verschillende talen deze taak anders af - sommige hebben ingebouwde functies (zoals `strip` in Python), terwijl anderen, zoals C, handmatige implementatie vereisen vanwege de focus op het geven van lagere-level controle aan ontwikkelaars.

Alternatieven omvatten het gebruik van bibliotheekfuncties zoals `strpbrk` om aanhalingstekens te vinden of het gebruik van reguliere expressies (met bibliotheken zoals PCRE) voor complexere patronen, hoewel dit overkill kan zijn voor het simpelweg verwijderen van aanhalingstekens.

De implementatie hierboven scant eenvoudig door elk karakter in de string, waarbij alleen niet-aanhalingstekenkarakters naar de schrijflocatie van de pointer worden gekopieerd. Dit is efficiënt omdat het ter plaatse wordt gedaan zonder extra geheugen nodig te hebben voor de resultaatstring.

## Zie ook

- [C Standard Library Functies](http://www.cplusplus.com/reference/clibrary/)
- [PCRE - Perl Compatible Regular Expressions](https://www.pcre.org/)
- [Pointers in C begrijpen](https://www.learn-c.org/en/Pointers)
