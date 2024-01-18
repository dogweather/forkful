---
title:                "Analisi della data da una stringa."
html_title:           "PowerShell: Analisi della data da una stringa."
simple_title:         "Analisi della data da una stringa."
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Parsing di una data da una stringa è il processo di estrarre una data scritta in formato testo (come "5 settembre 2021") e convertirla in un formato utilizzabile per il computer. I programmatori spesso si occupano di questo perché molti dati vengono immessi in quelle che sono essenzialmente liste di parole (come un'email di conferma di prenotazione con la data e l'ora della prenotazione), e vogliono essere in grado di estrarre le informazioni più importanti da queste stringhe.

## Come fare:
```PowerShell
$stringa = "5 settembre 2021"
$data = Get-Date -Date $stringa -Format "dd MMMM yyyy"
$data
```

Output:
```
05 settembre 2021
```

In questo esempio, abbiamo una stringa contenente una data in formato testuale italiano. Per convertirla in un formato utilizzabile, utilizzeremo la funzione `Get-Date` di PowerShell con il parametro `-Format` per specificare il formato della data desiderato. Se non specificato, il formato sarà quello predefinito del computer. In questo caso, abbiamo scelto un formato che includa il giorno del mese scritto per esteso ("dd"), il nome del mese completo ("MMMM") e l'anno in formato numerico ("yyyy"). Il risultato è una data formattata correttamente, pronta per essere utilizzata dai nostri script.

È importante notare che il formato della data può variare in base alla lingua e alle impostazioni del computer. Ad esempio, se il vostro computer è impostato in inglese, la forma analogica del comando sarebbe: `Get-Date -Date $stringa -Format "MMMM dd yyyy"`. È sempre consigliabile testare il formato della data con una stringa di esempio prima di integrarlo nel vostro script.

## Approfondimento:
Parsing di una data da una stringa è sempre stato un'operazione importante nel mondo della programmazione. In passato, spesso si doverva scrivere codice personalizzato per ogni tipo di formato data, rendendo il processo lungo e noioso. Tuttavia, con l'avvento di linguaggi di programmazione moderni, come PowerShell, il parsing di una data da una stringa è diventato più semplice grazie a funzioni e comandi dedicati.

Esistono anche molte alternative per il parsing di una data da una stringa, tra cui l'utilizzo di librerie esterne, come `DateTime.ParseExact` in .NET Framework o il modulo `dateutil` in Python. Tuttavia, con l'utilizzo di PowerShell, non è necessario installare alcuna libreria aggiuntiva e il processo è abbastanza semplice da gestire utilizzando le funzioni native del linguaggio.

Per chi vuole approfondire il processo di parsing di una data da una stringa in PowerShell, esistono molte fonti online con esempi più avanzati e metodi alternativi da esplorare.

## Vedi anche:
- [Documentazione di PowerShell su Get-Date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [Libreria DateTime.ParseExact di .NET Framework](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parseexact?view=net-5.0)
- [Modulo dateutil per Python](https://dateutil.readthedocs.io/en/stable/)