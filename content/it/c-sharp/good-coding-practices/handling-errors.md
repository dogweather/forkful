---
date: 2024-01-26 00:50:10.744079-07:00
description: "Gestire gli errori in C# significa gestire l'inaspettato\u2014come inciampare\
  \ sui propri lacci delle scarpe. I programmi possono inciampare su dati errati o\u2026"
lastmod: '2024-03-13T22:44:43.441044-06:00'
model: gpt-4-1106-preview
summary: "Gestire gli errori in C# significa gestire l'inaspettato\u2014come inciampare\
  \ sui propri lacci delle scarpe."
title: Gestione degli errori
weight: 16
---

## Cosa & Perché?

Gestire gli errori in C# significa gestire l'inaspettato—come inciampare sui propri lacci delle scarpe. I programmi possono inciampare su dati errati o connessioni difettose. Gestiamo gli errori per evitare che il nostro software cada di faccia, permettendogli di riprendersi agilmente.

## Come fare:

Cominciamo con un blocco try-catch. È come mettere una rete di sicurezza sotto un funambolo. Se scivola, non precipita—viene catturato.

```C#
using System;

class EsempioGestioneErrori {
    static void Main() {
        try {
            int[] numeri = {1, 2, 3};
            Console.WriteLine(numeri[5]);  // Ops, l'indice è fuori dal limite!
        } catch (IndexOutOfRangeException e) {
            Console.WriteLine("Errore catturato: " + e.Message);
        }
    }
}
```

Esempio di output quando le cose vanno male:
```
Errore catturato: Indice non compreso nei limiti dell'array.
```

Ora aggiungiamo un blocco finally—succede comunque, come pagare le tasse.

```C#
try {
    // Qui codice potenzialmente problematico
} catch (SomeSpecificException e) {
    // Gestire qui quell'errore specifico
} finally {
    // Questo codice viene eseguito a prescindere da quello che succede sopra
    Console.WriteLine("Questo viene sempre eseguito.");
}
```

## Approfondimento

La gestione degli errori è presente in C# fin dalla sua nascita. Col tempo, si è evoluta. In passato, i programmatori si affidavano a codici di ritorno o flag globali per segnalare problemi—ingombranti e soggetti ad errori.

C# utilizza le eccezioni, un approccio più moderno. Un'eccezione viene lanciata quando succede l'inaspettato, proprio come lanciare una bandierina in una partita di football. La gestione strutturata delle eccezioni con blocchi try, catch e finally rende la gestione di questi momenti più chiara e pulita rispetto ai vecchi metodi di controllo degli errori.

Alternative? Certo. C'è `UnhandledExceptionEventHandler` per le eccezioni che sfuggono. Oppure, nel codice asincrono, la gestione degli errori diventa un po' al contrario con gli oggetti `Task` che portano con sé il loro bagaglio di eccezioni.

I dettagli dell'implementazione—paragonabili al carattere piccolo—contano. Le eccezioni possono essere costose, riducendo le prestazioni se lanciate all'impazzata. Quindi, le utilizziamo solo per casi eccezionali, non per il controllo logico di tutti i giorni.

## Vedi Anche

- [Documentazione ufficiale sulle Eccezioni in C#](https://docs.microsoft.com/it-it/dotnet/csharp/fundamentals/exceptions/exception-handling)
- [Migliori pratiche nella gestione delle eccezioni in C#](https://docs.microsoft.com/it-it/dotnet/standard/exceptions/best-practices-for-exceptions)
