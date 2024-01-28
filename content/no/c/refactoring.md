---
title:                "Refaktorering"
date:                  2024-01-26T01:16:45.602502-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorering"
programming_language: "C"
category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/refactoring.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Refaktorering er prosessen med å restrukturere eksisterende dataprogramkode uten å endre dens eksterne oppførsel. Programmerere gjør dette for å forbedre lesbarheten, redusere kompleksiteten, eller gjøre koden mer vedlikeholdbar og skalerbar, noe som kan spare en lastebilfull av tid og hodebry senere.

## Hvordan:
La oss friske opp litt kode. Forestill deg at du har en funksjon som beregner gjennomsnittet av heltall i et array. Ved første øyekast er det litt av et rot.

**Før Refaktorering:**
```C
#include <stdio.h>

double calculateStuff(int numbers[], int size) {
    int sum = 0;
    double average;
    for(int i = 0; i < size; sum += numbers[i++]); // Summerer i for-løkkebetingelsen, au!
    average = (double)sum / size;
    return average;
}

int main() {
    int array[] = {23, 42, 57, 16, 5, 102};
    int length = sizeof(array) / sizeof(array[0]);
    printf("Gjennomsnitt: %f\n", calculateStuff(array, length));

    return 0;
}
```

**Etter Refaktorering:**
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
    printf("Gjennomsnitt: %f\n", calculateAverage(array, length));
    return 0;
}
```
Selv med dette enkle eksempelet kan du se hvordan splittingen av funksjonen gjør koden renere og mer vedlikeholdbar. Hver funksjon har nå ett enkelt ansvar – et nøkkelprinsipp i ren kode.

## Dypdykk
Begrepet "refaktorering" ble populært på slutten av 90-tallet, spesielt med publiseringen av Martin Fowlers bok "Refaktorering: Forbedring av designet på eksisterende kode". Refaktorering innebærer ikke å fikse bugs eller legge til nye funksjoner, men det handler om å forbedre strukturen av koden.

Det finnes mange flotte verktøy for refaktorering og integrerte utviklingsmiljøer (IDEer) som hjelper med å automatisere prosessen, som CLion for C og C++, men det å forstå hva som foregår under hetten er fortsatt avgjørende.

Alternativer til refaktorering kan inkludere å skrive om koden fra bunnen av (risikabelt og ofte unødvendig) eller leve med teknisk gjeld (som kan bli dyrere i det lange løp). Implementeringsdetaljer varierer basert på prosjektet, men vanlige refaktoreringer inkluderer å gi variabler nye navn for klarhet, bryte opp store funksjoner i mindre, og erstatte magiske tall med navngitte konstanter.

Også, mønstre som DRY (Don't Repeat Yourself) og SOLID-prinsipper kan veilede refaktoreringsreisen din, og skyve for en kodebase som er lettere å teste, forstå, og samarbeide på.

## Se Også
For å dykke dypere inn i refaktoreringens verden, ta en titt på:

- Martin Fowlers hjemmeside: https://martinfowler.com/ med en skattekiste av artikler og ressurser om refaktorering og programvaredesign.
- Refactoring.com: https://refactoring.com/ gir eksempler og kataloger av refaktoreringsteknikker.
- "Refaktorering"-boken: Ansett som en bibel for refaktorering, å lese den gir deg en komplett oversikt over metodikken.
- "Ren kode: En håndbok i smidig programvarehåndverk" av Robert C. Martin, som diskuterer hvordan man skriver kode som er lett å forstå og vedlikeholde.
