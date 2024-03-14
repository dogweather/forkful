---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:40.808448-07:00
description: "Een debugger gebruiken betekent dat je gespecialiseerde tools gebruikt\
  \ om code te testen en te diagnosticeren. Programmeurs doen dit om bugs te pletten,\u2026"
lastmod: '2024-03-13T22:44:50.814773-06:00'
model: gpt-4-0125-preview
summary: "Een debugger gebruiken betekent dat je gespecialiseerde tools gebruikt om\
  \ code te testen en te diagnosticeren. Programmeurs doen dit om bugs te pletten,\u2026"
title: Een debugger gebruiken
---

{{< edit_this_page >}}

## Wat & Waarom?
Een debugger gebruiken betekent dat je gespecialiseerde tools gebruikt om code te testen en te diagnosticeren. Programmeurs doen dit om bugs te pletten, de stroom van code te begrijpen en te zorgen dat hun code zich gedraagt zoals verwacht - het is alsof je een microscoop voor de hersenen van je code hebt.

## Hoe te:
Stel je voor dat je een klein programma hebt dat zich niet goed gedraagt:

```C#
static void Main()
{
    int resultaat = Som(1, 2);
    Console.WriteLine(resultaat);
}

static int Som(int a, int b)
{
    return a + a; // Oeps, moet a + b zijn
}
```

Gebruikmakend van de debugger van Visual Studio, stel een breekpunt in door te klikken op de linker marge naast `return a + a;`. Wanneer je het programma uitvoert (met F5), zal de uitvoering daar pauzeren. Beweeg over variabelen om hun waarden te inspecteren, of gebruik het Immediate Window om uitdrukkingen te evalueren. Je zult zien dat `a` 1 is en `b` 2, maar `a + a` is niet onze verwachte som. Verander het naar `a + b`, ga verder met uitvoeren (F5), en voila, de console geeft 3 uit.

## Diepgaand
De geschiedenis van debugging gaat helemaal terug naar de jaren 1940 toen een echte bug (een mot) werd gevonden in een vroege computer. De debuggers van tegenwoordig, zoals die in Visual Studio, bieden een reeks krachtige functies, waaronder breekpunten, stap-voor-stap uitvoering, watch windows, en meer.

Alternatieven voor de debugger van Visual Studio zijn onder andere open-source opties zoals GDB voor C-stijl talen of pdb voor Python, en cross-platform IDE's zoals JetBrains Rider of VS Code die debuggingtools bieden voor C# en andere talen.

Wanneer je duikt in de implementatie van een debugger, kijk je naar een programma dat zich hecht aan het proces van je applicatie. Het interpreteert machinecode, beheert de status van het geheugen, en controleert de uitvoeringsstroom. Dit is zware kost die cruciaal is voor effectieve debugging, wat de reden is waarom de debugmodus vaak langzamer loopt dan de release modus waar deze haakjes niet bestaan.

## Zie Ook
- [Documentatie van Visual Studio Debugger](https://docs.microsoft.com/en-us/visualstudio/debugger/)
- [Debugging Strategieën](https://www.codeproject.com/Articles/79508/Effective-Exception-Handling-in-Visual-C)
