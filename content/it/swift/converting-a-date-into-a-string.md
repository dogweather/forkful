---
title:                "Convertire una data in una stringa"
html_title:           "Swift: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Una delle attività più comuni all'interno di un'applicazione è la conversione di una data in una stringa. Questo è utile quando si vuole mostrare la data in un formato specifico o salvarla in un formato compatibile con un database o un server esterno.

## Come Fare

Per convertire una data in una stringa in Swift, è necessario utilizzare il tipo di dati "DateFormatter". Questo ci permette di specificare il formato desiderato per la nostra stringa.

````Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy" //specificare il formato della data desiderato
let date = Date() //ottenere la data corrente
let dateString = dateFormatter.string(from: date) //convertire la data in una stringa
print(dateString) // output: 27/08/2021
````

In questo esempio, abbiamo ottenuto la data corrente utilizzando il tipo "Date" e l'abbiamo convertita in una stringa utilizzando il formato "dd/MM/yyyy" (giorno/mese/anno). È possibile utilizzare diverse combinazioni di lettere per specificare formati diversi, come "MM/dd/yyyy" o "yyyy-MM-dd".

## Approfondimento

La classe "DateFormatter" dispone di molte altre funzionalità per gestire la conversione di date in stringhe. Ad esempio, è possibile specificare un linguaggio o una localizzazione per adattare il formato della data alle convenzioni di un paese specifico. Inoltre, è possibile anche convertire una stringa in una data utilizzando lo stesso formatter.

Per ulteriori informazioni su come utilizzare "DateFormatter" e altre funzionalità per gestire le date in Swift, si consiglia di consultare la documentazione ufficiale di Apple: https://developer.apple.com/documentation/foundation/dateformatter.

## Vedi Anche

- https://www.swiftbysundell.com/basics/date-and-time/
- https://www.hackingwithswift.com/example-code/system/how-to-parse-dates-from-a-string-using-dateformatter