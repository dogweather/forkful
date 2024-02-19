---
aliases:
- /it/vba/getting-the-current-date/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:38.684936-07:00
description: "In Visual Basic for Applications (VBA), recuperare la data corrente\
  \ \xE8 un compito comune che permette ai programmatori di lavorare dinamicamente\
  \ con le\u2026"
lastmod: 2024-02-18 23:08:55.734176
model: gpt-4-0125-preview
summary: "In Visual Basic for Applications (VBA), recuperare la data corrente \xE8\
  \ un compito comune che permette ai programmatori di lavorare dinamicamente con\
  \ le\u2026"
title: Ottenere la data corrente
---

{{< edit_this_page >}}

## Cosa & Perché?

In Visual Basic for Applications (VBA), recuperare la data corrente è un compito comune che permette ai programmatori di lavorare dinamicamente con le date nei loro macro o applicazioni. Questa funzionalità è fondamentale per operazioni come il logging, la marcatura temporale delle transazioni o il calcolo basato sulle date.

## Come fare:

Recuperare la data corrente in VBA è semplice, utilizzando la funzione `Date`, mentre la funzione `Now` fornisce sia la data corrente che l'ora. Ecco come puoi lavorare con entrambe:

```vb
Sub GetCurrentDate()
    ' Utilizzando la funzione Date per ottenere la data corrente
    Dim currentDate As Date
    currentDate = Date
    Debug.Print "Data Corrente: "; currentDate
    
    ' Utilizzando la funzione Now per ottenere la data e l'ora correnti
    Dim currentDateTime As Date
    currentDateTime = Now
    Debug.Print "Data e Ora Correnti: "; currentDateTime
End Sub
```

Quando esegui questo macro, il metodo `Debug.Print` visualizza la data corrente e la data e l'ora correnti nella Finestra Immediata nell'editor VBA. Ad esempio:

```
Data Corrente: 12/4/2023
Data e Ora Correnti: 12/4/2023 15:45:22
```

Tieni presente che il formato della data potrebbe variare in base alle impostazioni di sistema del computer dell'utente.

## Approfondimento

Le funzioni `Date` e `Now` incapsulano la complessità di gestire date e orari in Visual Basic for Applications, fornendo un'astrazione a livello di applicazione che rende il lavoro con le date semplice e intuitivo. Storicamente, gestire date e orari nella programmazione è stato pieno di sfide, inclusa la gestione di diversi fusi orari, cambiamenti dell'ora legale e vari formati di data.

In VBA, queste funzioni si basano sulla data e sull'ora del sistema sottostante, il che significa che sono influenzate dalle impostazioni locali e di sistema dell'utente. È una spada a doppio taglio che garantisce coerenza con l'ambiente dell'utente, ma richiede anche un'attenta gestione della localizzazione e degli aggiustamenti del fuso orario in applicazioni globali.

Sebbene le funzioni di data e ora di VBA siano perfettamente adatte per molte applicazioni, soprattutto nell'ambito dell'automazione di Office, potrebbero mancare di precisione o granularità richieste per applicazioni più complesse come sistemi di trading ad alta frequenza o simulazioni scientifiche. In tali casi, altri ambienti di programmazione o linguaggi come Python o C# potrebbero offrire librerie di manipolazione di date e ore più sofisticate.

Tuttavia, per la stragrande maggioranza dei compiti che coinvolgono date e orari nel contesto di Excel, Word o altre applicazioni Office, le funzioni `Date` e `Now` di VBA offrono un equilibrio di semplicità, prestazione e facilità d'uso difficile da superare.
