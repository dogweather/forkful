---
title:                "Refaktorisering"
date:                  2024-01-26T01:16:40.252436-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorisering"
programming_language: "C"
category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/refactoring.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Refaktorisering är processen att omstrukturera befintlig dator kod utan att ändra dess yttre beteende. Programmerare gör det för att förbättra läsbarheten, minska komplexiteten eller göra koden mer underhållsbar och skalbar, vilket kan spara en hel del tid och huvudvärk längre fram.

## Hur gör man:
Låt oss fräscha upp lite kod. Tänk dig att du har en funktion som beräknar medelvärdet av heltal i en array. Vid första anblicken är det lite av en röra.

**Före refaktorisering:**
```C
#include <stdio.h>

double calculateStuff(int numbers[], int size) {
    int sum = 0;
    double average;
    for(int i = 0; i < size; sum += numbers[i++]); // Summation i for-loops villkoret, aj då!
    average = (double)sum / size;
    return average;
}

int main() {
    int array[] = {23, 42, 57, 16, 5, 102};
    int längd = sizeof(array) / sizeof(array[0]);
    printf("Medelvärde: %f\n", calculateStuff(array, längd));

    return 0;
}
```

**Efter refaktorisering:**
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
    int längd = sizeof(array) / sizeof(array[0]);
    printf("Medelvärde: %f\n", calculateAverage(array, längd));
    return 0;
}
```
Även med det här enkla exemplet kan du se hur uppdelningen av funktionen gör koden renare och mer underhållsbar. Varje funktion har nu ett enda ansvar – en nyckelprincip i ren kodning.

## Djupdykning
Termen "refaktorisering" populariserades i slutet av 90-talet, särskilt med publiceringen av Martin Fowlers bok "Refactoring: Improving the Design of Existing Code." Refaktorisering innebär inte att fixa buggar eller lägga till nya funktioner, utan det handlar om att förbättra kodens struktur.

Det finns många toppmoderna refaktoriseringsverktyg och IDE:er (Integrerade Utvecklingsmiljöer) som hjälper till att automatisera processen, som CLion för C och C++, men att förstå vad som händer under huven är fortfarande avgörande.

Alternativ till refaktorisering kan inkludera att skriva om kod från grunden (riskabelt och ofta onödigt) eller leva med den tekniska skulden (vilket kan vara dyrare i längden). Implementeringsdetaljer varierar beroende på projektet, men vanliga refaktoriseringar inkluderar att byta namn på variabler för tydlighet, bryta ned stora funktioner i mindre delar, och ersätta magiska nummer med namngivna konstanter.

Dessutom kan mönster som DRY (Don't Repeat Yourself) och SOLID-principer vägleda din refaktoriseringsresa, mot en kodbas som är lättare att testa, förstå och samarbeta med.

## Se även
För att dyka djupare in i refaktoriseringshavet, ta en titt på:

- Martin Fowlers hemsida: https://martinfowler.com/ med en skattkista av artiklar och resurser om refaktorisering och mjukvarudesign.
- Refactoring.com: https://refactoring.com/ erbjuder exempel och kataloger över refaktoriseringsmetoder.
- Boken "Refactoring": Ansedd som en bibel för refaktorisering, genom att läsa den får du en komplett vy av metoden.
- "Clean Code: A Handbook of Agile Software Craftsmanship" av Robert C. Martin, som diskuterar att skriva kod som är lätt att förstå och underhålla.