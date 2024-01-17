---
title:                "Ottenere la data corrente."
html_title:           "PowerShell: Ottenere la data corrente."
simple_title:         "Ottenere la data corrente."
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
Ottenere la data corrente è il processo di ottenere la data e l'ora attuali dal sistema operativo. I programmatori spesso lo fanno per monitorare le prestazioni del sistema, generare rapporti o per qualsiasi altra operazione che richieda l'utilizzo della data corrente.

## Come fare:
Ci sono diverse opzioni per ottenere la data corrente in PowerShell:

1. Utilizzare il cmdlet `Get-Date` per ottenere la data e l'ora correnti nel formato predefinito.
```
PowerShell Get-Date

Output: 3/10/2021 9:15:00 AM
```

2. Utilizzare il parametro `-Format` per specificare un formato personalizzato.
```
PowerShell Get-Date -Format "dd/MM/yyyy"

Output: 10/03/2021
```

3. Usare il cmdlet `Get-Date -DisplayHint Date` per visualizzare solo la data senza l'ora.
```
PowerShell Get-Date -DisplayHint Date

Output: 3/10/2021
```

## Approfondimento:
Per ottenere la data corrente, PowerShell utilizza il clock del sistema operativo. Tuttavia, si può anche ottenere la data di un fuso orario specifico utilizzando il parametro `-UFormat` e specificando il formato dei fusi orari UNIX.

Inoltre, è possibile utilizzare il cmdlet `Get-Culture` per ottenere il formato di data e ora impostato nel sistema corrente e utilizzarlo per formattare la data corrente in base alle preferenze locali.

## Vedi anche:
- [Documentazione Microsoft su Get-Date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [Documentazione Microsoft su Get-Culture](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-culture?view=powershell-7.1)