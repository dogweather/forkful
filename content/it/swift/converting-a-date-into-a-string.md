---
title:                "Convertire una data in una stringa"
html_title:           "Javascript: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
La conversione di una data in una stringa è un processo comune nel mondo della programmazione, infatti, è proprio così che trasformiamo un valore di tipo `Date` (data) in un valore di tipo `String` (stringa). Questa operazione è molto utile quando vogliamo visualizzare una data in un formato specifico o come parte di un output più complesso.

## Come Fare:
Ecco un esempio di come trasformare una data in una stringa usando Swift. 

```Swift
import Foundation

let adesso = Date()
let formatter = DateFormatter()
formatter.dateFormat = "dd.MM.yyyy"
let dataComeStringa = formatter.string(from: adesso)
print(dataComeStringa)  //Stampa la data del giorno in formato "gg.mm.aaaa"
```

## Approfondimento
1. **Contesto storico:** La necessità di convertire una data in una stringa non è nuova, si tratta di una pratica comune in tutte le lingue di programmazione. In Swift, questa funzione è stata resa più agevole con l'introduzione della classe `DateFormatter` in Swift 4.

2. **Alternative:** In Swift, `DateFormatter` è il modo più comune per la conversione, ma esistono altre opzioni. Ad esempio, si può utilizzare la funzione `dateStyle` e `timeStyle` per formati di data predefiniti.

```Swift
import Foundation

let adesso = Date()
let formatter = DateFormatter()
formatter.dateStyle = .medium
formatter.timeStyle = .none
let dataComeStringa2 = formatter.string(from: adesso)
print(dataComeStringa2)  //Stampa la data del giorno in un formato predefinito
```

3. **Dettagli di implementazione:** Per rendere la data leggibile in diversi formati, `DateFormatter` utilizza i simboli di formato di data e ora definiti dall'Unicode Technical Standard #35.

## Vedi Anche
1. Documentazione Apple su `DateFormatter`: https://developer.apple.com/documentation/foundation/dateformatter
2. Unicode Technical Standard #35: https://www.unicode.org/reports/tr35/
3. Apple Developer Guide al formato delle date: https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DataFormatting/Articles/dfDateFormatting10_4.html#//apple_ref/doc/uid/TP40002369-SW1