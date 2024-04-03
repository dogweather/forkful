---
date: 2024-01-26 01:09:33.578591-07:00
description: "Come fare: Immagina di avere un codice che stampa un saluto pi\xF9 volte.\
  \ Senza funzioni, \xE8 un disordine. Con le funzioni, \xE8 ordinato."
lastmod: '2024-03-13T22:44:43.439178-06:00'
model: gpt-4-1106-preview
summary: "Immagina di avere un codice che stampa un saluto pi\xF9 volte."
title: Organizzazione del codice in funzioni
weight: 18
---

## Come fare:
Immagina di avere un codice che stampa un saluto più volte. Senza funzioni, è un disordine. Con le funzioni, è ordinato.

```C#
// Senza funzioni - ripetitivo
Console.WriteLine("Ciao, Amy!");
Console.WriteLine("Ciao, Bob!");
Console.WriteLine("Ciao, Charlie!");

// Con funzioni - più pulito
void Saluta(string nome) {
    Console.WriteLine($"Ciao, {nome}!");
}

Saluta("Amy");
Saluta("Bob");
Saluta("Charlie");
```

L'output è lo stesso, ma la seconda versione è molto più ordinata.

## Approfondimento
Tempo fa, ai tempi del linguaggio assembly, si saltava a differenti parti di codice con GOTO—caotico e difficile da tracciare. Le funzioni sono un salto di qualità notevole, come i cassetti organizzati in una scatola degli attrezzi. Alternative? Certo. Hai metodi, che sono funzioni in un contesto di classe. Poi ci sono le lambda e le funzioni inline per compiti rapidi e una tantum.

Riguardo all'implementazione—le funzioni piccole e focalizzate sono oro. Sono più facili da testare e da correggere. Grandi funzioni con molte responsabilità possono diventare mostri, guadagnandosi il dubbioso titolo di "codice spaghetti". Attieniti a un compito per funzione; te ne sarai grato più tardi.

## Vedi anche
Per ulteriori informazioni sulle funzioni e le migliori pratiche, consulta:

- Clean Code di Robert C. Martin: Principi per mantenere ordinate le tue funzioni.
- Refactoring di Martin Fowler: Modi per migliorare il codice esistente.
- Guida Microsoft C# sui Metodi: https://docs.microsoft.com/it-it/dotnet/csharp/programming-guide/classes-and-structs/methods
