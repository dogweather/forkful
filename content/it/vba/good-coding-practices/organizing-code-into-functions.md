---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:30.693651-07:00
description: "Organizzare il codice in funzioni in Visual Basic for Applications (VBA)\
  \ comporta la suddivisione di un programma in pezzi pi\xF9 piccoli e gestibili,\
  \ noti\u2026"
lastmod: '2024-03-13T22:44:43.269206-06:00'
model: gpt-4-0125-preview
summary: "Organizzare il codice in funzioni in Visual Basic for Applications (VBA)\
  \ comporta la suddivisione di un programma in pezzi pi\xF9 piccoli e gestibili,\
  \ noti come funzioni."
title: Organizzare il codice in funzioni
weight: 18
---

## Come fare:
In VBA, le funzioni vengono definite utilizzando le istruzioni `Function` e `End Function`. Ecco un semplice esempio di come creare una funzione che calcola l'area di un rettangolo:

```basic
Function CalculateArea(lunghezza As Double, larghezza As Double) As Double
    CalculateArea = lunghezza * larghezza
End Function
```

Per chiamare questa funzione nel tuo codice VBA e visualizzare il risultato in un message box, useresti:

```basic
Sub ShowArea()
    Dim area As Double
    area = CalculateArea(10, 5)
    MsgBox "L'area è " & area
End Sub
```

Quando eseguito, questo codice visualizza un message box che afferma: `L'area è 50`.

### Passaggio di Variabili ByRef e ByVal
VBA consente di passare variabili alle funzioni sia per riferimento (`ByRef`) che per valore (`ByVal`). Il primo significa che la variabile originale può essere modificata dalla funzione, mentre il secondo passa una copia, proteggendo la variabile originale da modifiche.

```basic
Function ModifyValue(ByRef num As Integer)
    num = num + 5
End Function

Function PreserveValue(ByVal num As Integer) As Integer
    num = num + 5
    PreserveValue = num
End Function
```

## Approfondimento
VBA, come linguaggio di programmazione guidato dagli eventi, pone un'enfasi significativa su funzioni e subroutine per gestire vari compiti. A differenza di molti linguaggi moderni, VBA ha una caratteristica unica dove la parola chiave `Function` non solo dichiara un blocco di codice riutilizzabile ma consente anche un valore di ritorno implicito assegnato direttamente al nome della funzione.

Storicamente, la progettazione delle funzioni VBA è stata influenzata da paradigmi di programmazione precedenti in cui l'incapsulamento e la modularità venivano gradualmente riconosciuti per la loro importanza nello sviluppo del software. Questo contesto storico ha portato VBA ad adottare un approccio alla organizzazione del codice conservativo eppure funzionale.

Sebbene VBA sia potente all'interno dei suoi ambienti nativi (ad es., applicazioni Microsoft Office), è essenziale notare che il mondo della programmazione si è evoluto. Linguaggi come Python offrono una sintassi più semplice e una vasta libreria standard, rendendoli un'alternativa favorevole per varie applicazioni al di fuori della suite Office. Tuttavia, quando si lavora all'interno dei prodotti Microsoft Office, le capacità di integrazione e automazione che VBA fornisce sono ineguagliate.

Vale la pena notare che, nonostante la sua età, la comunità intorno a VBA rimane attiva, trovando continuamente modi innovativi per sfruttarne le funzionalità. Eppure, poiché l'industria del software si muove verso linguaggi più moderni, versatili e robusti, si incoraggiano i programmatori familiari con VBA a esplorare queste alternative per compiti non legati a Office al fine di ampliare il loro kit di strumenti di programmazione.
