---
aliases:
- /it/vba/concatenating-strings/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:50:24.332850-07:00
description: "La concatenazione in Visual Basic for Applications (VBA) implica l'unione\
  \ di due o pi\xF9 stringhe in un'unica entit\xE0. Questo \xE8 un compito fondamentale\
  \ nella\u2026"
lastmod: 2024-02-18 23:08:55.714122
model: gpt-4-0125-preview
summary: "La concatenazione in Visual Basic for Applications (VBA) implica l'unione\
  \ di due o pi\xF9 stringhe in un'unica entit\xE0. Questo \xE8 un compito fondamentale\
  \ nella\u2026"
title: Concatenazione di stringhe
---

{{< edit_this_page >}}

## Cosa & Perché?

La concatenazione in Visual Basic for Applications (VBA) implica l'unione di due o più stringhe in un'unica entità. Questo è un compito fondamentale nella programmazione, essenziale per generare messaggi per l'utente, creare query SQL e altro ancora, poiché consente la creazione e la manipolazione dinamica dei dati di stringa.

## Come fare:

VBA fornisce un metodo semplice per concatenare le stringhe utilizzando l'operatore `&` o la funzione `Concatenate`. Esploriamo entrambi i metodi con esempi:

1. **Utilizzando l'operatore `&`:**

L'operatore `&` è il metodo più comune per concatenare le stringhe in VBA. È semplice ed efficiente per unire più stringhe.

```vb.net
Dim firstName As String
Dim lastName As String
firstName = "Jane"
lastName = "Doe"
' Concatenazione di stringhe
Dim fullName As String
fullName = firstName & " " & lastName
Debug.Print fullName 'Output: Jane Doe
```

2. **Utilizzando la funzione `Concatenate`:**

In alternativa, VBA permette la concatenazione di stringhe utilizzando la funzione `Concatenate`, particolarmente utile quando si ha a che fare con un array di stringhe o quando si preferisce una sintassi di funzione.

```vb.net
Dim greetings As String
Dim name As String
greetings = "Ciao"
name = "John"
' Concatenazione di stringhe usando la funzione Concatenate
Dim message As String
message = Application.WorksheetFunction.Concatenate(greetings, " ", name, "!")
Debug.Print message 'Output: Ciao John!
```

La scelta tra l'operatore `&` e la funzione `Concatenate` dipende dalle preferenze personali e dai requisiti specifici del proprio progetto.

## Approfondimento

La concatenazione di stringhe è una caratteristica di base ma potente in VBA, che trae le sue origini dai primi linguaggi di programmazione. La prevalenza dell'operatore `&` in VBA per la concatenazione rispetto all'operatore `+`, comunemente usato in molti altri linguaggi, sottolinea l'attenzione di VBA per un'esplicita gestione delle stringhe, evitando così incompatibilità e errori involontari nei tipi di dati.

Mentre l'operatore `&` è efficiente e ampiamente adottato, la funzione `Concatenate` brilla in scenari che richiedono più chiarezza o che gestiscono casi speciali di concatenazione, come quando si ha a che fare con array. Tuttavia, è importante notare che le versioni moderne di Excel hanno introdotto la funzione `TEXTJOIN`, che può essere più efficiente per concatenare array di stringhe con un delimitatore, anche se non fa direttamente parte di VBA.

Quando si affrontano manipolazioni di stringhe estese o applicazioni critiche per le prestazioni, i programmatori potrebbero esplorare alternative come l'uso della classe `StringBuilder` in .NET (accessibile tramite COM in VBA). Questo può migliorare significativamente le prestazioni, particolarmente in loop o quando si concatenano un gran numero di stringhe, grazie ai suoi schemi di utilizzo della memoria più efficienti.

In definitiva, la scelta del metodo giusto per concatenare le stringhe in VBA dipende dalle proprie esigenze specifiche, considerazioni sulla prestazione e leggibilità. Che si opti per la semplicità dell'operatore `&` o per la funzionalità della funzione `Concatenate`, comprendere le implicazioni e l'efficienza di ciascun approccio è fondamentale per una manipolazione efficace delle stringhe in VBA.
