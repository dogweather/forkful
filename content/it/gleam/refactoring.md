---
title:                "Rifattorizzazione"
date:                  2024-01-26T01:18:11.504678-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rifattorizzazione"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/refactoring.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Il refactoring è il processo di rielaborazione del codice per renderlo più pulito, più manutenibile, senza alterarne il comportamento esterno. I programmatori rifattorizzano per migliorare la leggibilità, ridurre la complessità e rendere il codice più adatto ad aggiornamenti futuri o all'aggiunta di nuove funzionalità.

## Come fare:
Diciamo che hai un blocco di codice in cui stai facendo alcuni calcoli ripetuti o manipolazioni di stringhe attraverso più funzioni. Questo è un obiettivo primario per il refactoring. Ecco un prima e dopo usando Gleam, che pone una forte enfasi sulla sicurezza dei tipi e sull'immutabilità:

```gleam
// Prima del refactoring
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(width: Int, height: Int) {
  let area = calculate_area(width, height)
  io.println("L'area è \(area)")
}

// Dopo il refactoring
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(area: Int) {
  io.println("L'area è \(area)")
}

// In un'altra parte del tuo codice, chiamerai print_area così:
print_area(calculate_area(10, 20))
```

Output di esempio:
```
L'area è 200
```

Con il refactoring, abbiamo reso `print_area` più focalizzato solo sulla stampa, mentre il calcolo è gestito altrove, rendendo il codice più modulare e più facile da riutilizzare o testare.

## Approfondimenti
Il refactoring, come concetto, esiste da quando esiste la programmazione stessa—rivisitare e pulire il codice fa parte della buona gestione domestica. La formalizzazione moderna del refactoring, insieme a molte delle tecniche e dei modelli utilizzati oggi, può essere fatta risalire al libro fondamentale di Martin Fowler "Refactoring: Improving the Design of Existing Code" pubblicato nel 1999.

Nell'ecosistema Gleam, il refactoring ha considerazioni specifiche. Una delle più significative è il forte controllo dei tipi al momento della compilazione, che può aiutare a individuare gli errori in anticipo quando si spostano le cose. Le funzionalità di pattern matching e di immutabilità di Gleam possono anche guidarti a scrivere codice più chiaro e conciso—uno degli obiettivi principali del refactoring.

Le alternative al refactoring potrebbero includere la riscrittura del codice da zero o la correzione del codice con soluzioni rapide. Tuttavia, il refactoring è di solito l'approccio più sicuro ed efficiente per migliorare il codice esistente senza introdurre nuovi bug, poiché comporta trasformazioni incrementali, ben sottolineate, che preservano il comportamento.

## Vedi Anche
- Il libro "Refactoring" di Martin Fowler: https://martinfowler.com/books/refactoring.html
- Il sito web del linguaggio Gleam, con documentazione aggiuntiva ed esempi: https://gleam.run/
- "Refactoring: Improving the Design of Existing Code" di Martin Fowler (per i principi sottostanti applicabili a diversi linguaggi): https://martinfowler.com/books/refactoring.html
