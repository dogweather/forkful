---
title:                "Karakters verwijderen die overeenkomen met een patroon"
date:                  2024-01-28T21:58:22.738462-07:00
model:                 gpt-4-0125-preview
simple_title:         "Karakters verwijderen die overeenkomen met een patroon"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Karakters verwijderen die overeenkomen met een patroon in C gaat over het vinden en verwijderen van specifieke reeksen karakters uit strings. Programmeurs doen dit om input te desinfecteren, tekst te manipuleren of gegevens voor te bereiden voor verwerking.

## Hoe te:
Om karakters die overeenkomen met een patroon uit een string te verwijderen, kunnen we de `strpbrk` functie gebruiken om voorkomens te vinden en `strcpy` of `memmove` om tekst rond te schuiven. Hier is een snel voorbeeld:
```c
#include <stdio.h>
#include <string.h>

void delete_pattern(char *str, const char *pattern) {
    char *match;
    while ((match = strpbrk(str, pattern)) != NULL) {
        memmove(match, match + 1, strlen(match));
    }
}

int main() {
    char text[] = "Hello, World! Today is 2023.";
    delete_pattern(text, "o3!");
    printf("%s\n", text); // Uitvoer: Hell, Wrld Tday is 22.
    return 0;
}
```
Deze code speurt naar 'o', '3' en '!' karakters, en veegt ze uit de string.

## Diepgaand
Vroeger, voordat functies zoals `strpbrk` standaard waren, schreven coders vaak lussen die elk karakter tegen een patroon controleerden—vermoeiend maar noodzakelijk. De C-standaardbibliotheek van vandaag neemt veel van dat routinewerk weg, maar het is altijd goed om te begrijpen wat er onder de motorkap gebeurt.

`strpbrk` scant een string op de eerste overeenkomst in een set karakters, en `memmove` verplaatst bytes veilig rond, zelfs als ze overlappen. Dit is anders dan `strcpy`, dat niet goed kan omgaan met overlappend geheugen zonder haperingen.

Alternatieven omvatten regex-bibliotheken voor complexe patronen of handmatig lussen voor fijne controle. Maar het is altijd een afweging tussen het opnemen van externe bibliotheken of handgemaakte oplossingen voor prestatie- of geheugenbeperkingen.

## Zie Ook
- [C Stringbibliotheekfuncties](https://www.cplusplus.com/reference/cstring/)
