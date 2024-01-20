---
title:                "Analizzare una data da una stringa"
html_title:           "Fish Shell: Analizzare una data da una stringa"
simple_title:         "Analizzare una data da una stringa"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# ## Che cosa e perché?

Parse della data da una stringa è il processo di trasformazione di una rappresentazione di testo di una data in un equivalente comprensibile della macchina, spesso un oggetto DateTime. I programmatori lo fanno per manipolare le date in modi più complessi che semplicemente leggere o scrivere una stringa.

# ## Come fare:

Ecco come fare parsing di una data utilizzando PowerShell:

```PowerShell
$dataStringa = "2022-01-01"
$data = [DateTime]::Parse($dataStringa)
$data
```

E qui c'è l'output di esempio:

```PowerShell
Saturday, January 1, 2022 12:00:00 AM
```

Se invece la tua data è in un formato diverso, puoi utilizzare `[DateTime]::ParseExact` per specificare il formato:

```PowerShell
$dataStringa = "2022/01/01 13:15:00"
$formato = "yyyy/MM/dd HH:mm:ss"
$cultura = [Globalization.CultureInfo]::InvariantCulture
$data = [DateTime]::ParseExact($dataStringa, $formato, $cultura)
$data
```

L'output di esempio:

```PowerShell
Saturday, January 1, 2022 13:15:00 PM
```

# ## Approfondimento

Historicamente, PowerShell eredita la funzionalità del parser di date da .NET, che è noto per la sua flessibilità. Esistono molte alternative per analizzare le date. Oltre a `DateTime.Parse` e `DateTime.ParseExact`, ad esempio, potresti usare `DateTime.TryParse` e `DateTime.TryParseExact`, che non generano un'eccezione su una stringa mal formata, ma restituiscono un booleano di successo e assegnano l'oggetto DateTime ripristinato in un parametro di output.

Inoltre, il parsing delle date può essere influenzato da molte impostazioni, come la cultura corrente e la zona oraria del sistema. Ad esempio, la cultura corrente impatta sull'interpretazione del formato della data.

# ## Vedi anche

Di seguito alcuni link utili che potrebbero aiutarti a saperne di più:

1. Documentazione ufficiale Microsoft per l'uso di DateTime in PowerShell: [https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date)