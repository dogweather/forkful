---
title:                "Ottenere la data corrente"
html_title:           "Java: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Ottenere la data corrente in PowerShell significa accedere al preciso istante temporale in cui ti trovi, con tutti i dettagli, dal giorno all'ora. È un'operazione cruciale in programmazione perché permette l'organizzazione e il tracciamento di operazioni nel tempo.

## Come fare:

Ecco un semplice esempio su come ottenere la data corrente in PowerShell:

```PowerShell
$date = Get-Date
echo $date
```

L'output sarà del tipo:

```PowerShell
Martedì 26 Ottobre 2021 13:12:46
```

Nel caso in cui volessi ottenere una componente specifica, come solo il giorno o solo l'ora, ecco come fare:

```PowerShell
$day = (Get-Date).DayOfWeek
$time = (Get-Date).TimeOfDay

echo "Oggi è $day"
echo "L'ora corrente è $time"
```

Otterresti un output del genere:

```PowerShell
Oggi è Martedì
L'ora corrente è 13:12:46
```

## Approfondimento

Tieni presente che PowerShell usa l'oggetto .NET DateTime per ottenere la data corrente, il che significa che hai a tua disposizione tutti i metodi e le proprietà di quest'ultimo.

Storicamente, l'equivalente classico di Get-Date era la funzione 'date' in Unix Shell, benché quest'ultima presentasse meno opzioni.

Se non vuoi usare Get-Date, un'alternativa può essere l'operatore '-eq' con la parola chiave '$true'. Ad esempio:

```PowerShell
if ((Date).DayOfWeek -eq "Sunday") {$true} else {$false}
```

Questo codice restituirà 'true' se il giorno corrente è Domenica e 'false' in caso contrario.

## Vedi anche:

- Documentazione completa di Get-Date da Microsoft [(link)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date)
- ".NET DateTime Structure" da Microsoft [(link)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)