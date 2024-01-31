---
title:                "Refactoring"
date:                  2024-01-28T22:05:59.034464-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Refactoring is het proces van het herstructureren van bestaande computercode zonder het externe gedrag ervan te veranderen. Programmeurs doen dit om de leesbaarheid te verbeteren, de complexiteit te verminderen, of de code onderhoudbaarder en schaalbaarder te maken, wat op de lange termijn een vrachtlading aan tijd en hoofdpijn kan besparen.

## Hoe te:
Laten we wat code opknappen. Stel je voor dat je een functie hebt die het gemiddelde van gehele getallen in een array berekent. Op het eerste gezicht is het een beetje een wirwar.

**Voor Refactoring:**
```C
#include <stdio.h>

double calculateStuff(int numbers[], int size) {
    int sum = 0;
    double average;
    for(int i = 0; i < size; sum += numbers[i++]); // Optellen in de for-lusconditie, au!
    average = (double)sum / size;
    return average;
}

int main() {
    int array[] = {23, 42, 57, 16, 5, 102};
    int length = sizeof(array) / sizeof(array[0]);
    printf("Gemiddelde: %f\n", calculateStuff(array, length));

    return 0;
}
```

**Na Refactoring:**
```C
#include <stdio.h>

int calculateSum(const int numbers[], int size) {
    int sum = 0;
    for (int i = 0; i < size; ++i) {
        sum += numbers[i];
    }
    return sum;
}

double calculateAverage(const int numbers[], int size) {
    int sum = calculateSum(numbers, size);
    return (double)sum / size;
}

int main() {
    int array[] = {23, 42, 57, 16, 5, 102};
    int length = sizeof(array) / sizeof(array[0]);
    printf("Gemiddelde: %f\n", calculateAverage(array, length));
    return 0;
}
```
Zelfs met dit eenvoudige voorbeeld kun je zien hoe het opsplitsen van de functie de code schoner en onderhoudbaarder maakt. Elke functie heeft nu één verantwoordelijkheid - een sleutelprincipe in schone codering.

## Diepgaande Duik
De term "refactoring" werd in de late jaren 90 populair, vooral met de publicatie van Martin Fowlers boek "Refactoring: Improving the Design of Existing Code". Refactoring impliceert niet het repareren van bugs of het toevoegen van nieuwe functies, maar gaat over het verbeteren van de structuur van de code.

Er zijn veel gave refactoringtools en IDE's (Integrated Development Environments) die het proces helpen automatiseren, zoals CLion voor C en C++, maar begrijpen wat er onder de motorkap gebeurt blijft cruciaal.

Alternatieven voor refactoring kunnen onder meer het vanaf nul herschrijven van code (risicovol en vaak onnodig) of leven met de technische schuld (wat op de lange termijn kostbaarder kan zijn). Implementatiedetails variëren afhankelijk van het project, maar veelvoorkomende refactorings omvatten het hernoemen van variabelen voor meer duidelijkheid, het opsplitsen van grote functies in kleinere, en het vervangen van magische getallen door benoemde constanten.

Ook, patronen zoals DRY (Don't Repeat Yourself) en SOLID-principes kunnen je refactoring-reis leiden, en streven naar een codebase die gemakkelijker te testen, begrijpen en samen te werken is.

## Zie Ook
Om dieper in de zee van refactoring te duiken, kijk op:

- Martin Fowlers thuispagina: https://martinfowler.com/ met een schatkist aan artikelen en bronnen over refactoring en softwareontwerp.
- Refactoring.com: https://refactoring.com/ biedt voorbeelden en catalogi van refactorings technieken.
- Het boek "Refactoring": Beschouwd als een bijbel voor refactoring, geeft het lezen ervan je een compleet beeld van de methodologie.
- "Clean Code: A Handbook of Agile Software Craftsmanship" door Robert C. Martin, dat bespreekt hoe je code schrijft die gemakkelijk te begrijpen en te onderhouden is.
