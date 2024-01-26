---
title:                "Att organisera kod i funktioner"
date:                  2024-01-26T01:11:59.399504-07:00
model:                 gpt-4-1106-preview
simple_title:         "Att organisera kod i funktioner"
programming_language: "Python"
category:             "Python"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att organisera kod i funktioner handlar om att bryta ner din kod i återanvändbara delar med specifika syften. Vi gör det för att göra koden renare, enklare att läsa, felsöka och uppdatera.

## Hur gör man:
Låt säga att du skriver ett skript för att beräkna kvadraten och kuben av ett tal. Utan funktioner är det ett virrvarr av upprepning:

```Python
num = 4
square = num * num
cube = num * num * num
print(f"Kvadrat: {square}, Kub: {cube}")

num = 5
square = num * num
cube = num * num * num
print(f"Kvadrat: {square}, Kub: {cube}")
```
Utskrift:
```
Kvadrat: 16, Kub: 64
Kvadrat: 25, Kub: 125
```

Med funktioner är det snyggare:

```Python
def kvadrat(n):
    return n * n

def kub(n):
    return n ** 3

num = 4
print(f"Kvadrat: {kvadrat(num)}, Kub: {kub(num)}")

num = 5
print(f"Kvadrat: {kvadrat(num)}, Kub: {kub(num)}")
```
Utskrift:
```
Kvadrat: 16, Kub: 64
Kvadrat: 25, Kub: 125
```

## Fördjupning
Förr i tiden, när programmen var enkla, kunde man klara sig med att bara skriva en lista med instruktioner. Men när mjukvaran blev mer komplex insåg utvecklarna att de skrev om samma kod om och om igen. Hej på dig, funktioner—återanvändbara kodblock som utför en enda åtgärd.

Alternativ till funktioner inkluderar klasser (som knyter samman funktioner med data de opererar på) och inbäddad kod (intelligens precis där du behöver den, men riskabelt för komplexa uppgifter). När det gäller genomförandet är knepet inte bara att skapa funktioner, utan att få dem att göra en sak väl—tänk principen om enkelt ansvar. Funktioner bör också idealiskt vara tillståndslösa, vilket innebär inga överraskningar med data som kommer in eller går ut.

## Se också
- De officiella Python-handledningarna om funktioner: https://docs.python.org/3/tutorial/controlflow.html#defining-functions
- "Clean Code" av Robert C. Martin, för principer om hur man skriver rena funktioner.
- "Refactoring: Förbättring av designen av befintlig kod" av Martin Fowler, som inkluderar exempel på att organisera kod.