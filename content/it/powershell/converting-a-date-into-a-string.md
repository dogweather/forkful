---
title:                "Convertire una data in una stringa"
html_title:           "PowerShell: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
La conversione di una data in una stringa è il processo di trasformare una data in un formato leggibile e comprensibile per gli utenti. I programmatori lo fanno perché spesso devono mostrare una data in formato testuale all'interno dei loro script o applicazioni.

## Come fare:
```PowerShell
$date = Get-Date
$date.ToShortDateString()
$date.ToShortTimeString()
```
*Output:* `23/04/2020` 
`15:30`

## Approfondimento:
- Contesto storico: La necessità di convertire le date in formato testuale è nata con l'avvento dei computer, in quanto essi gestiscono le date in un formato diverso da quello umano. Grazie all'avanzare della tecnologia, oggi non è più necessario effettuare questa conversione manualmente, ma basta utilizzare le funzioni apposite dei linguaggi di programmazione.
- Alternative: Alcune alternative alla conversione di una data in stringa includono l'utilizzo di formattatori di date, che permettono di modificare il formato di visualizzazione di una data senza doverla convertire in una stringa.
- Dettagli di implementazione: In PowerShell, è possibile utilizzare le funzioni `ToShortDateString()` e `ToShortTimeString()` per ottenere la data convertita in formato testuale. È anche possibile personalizzare il formato della stringa utilizzando la funzione `ToString()` e specificando un pattern di formattazione.

## Vedi anche:
- [Documentazione ufficiale di Microsoft su Come convertire un valore di data in una stringa](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7#outputs)
- [Articolo di TechNet Magazine su Come formattare le date e l'ora in Windows PowerShell](https://technet.microsoft.com/en-us/library/dd315231.aspx)