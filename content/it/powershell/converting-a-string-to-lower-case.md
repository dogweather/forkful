---
title:                "Convertire una stringa in minuscolo"
html_title:           "Arduino: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Modificare una stringa in minuscolo significa convertire tutti i caratteri alfabetici presenti in una stringa in minuscoli. I programmatori lo fanno per standardizzare le input dell'utente, rendendo insensibili alla capitalizzazione le operazioni di confronto o ricerca.

## Come Fare:

Le seguenti opzioni di codice di PowerShell illustrano come convertire una stringa in minuscolo.

```PowerShell
$stringa = "Mario Rossi"
$stringaMinuscolo = $stringa.ToLower()
```

L'output sarà:

```PowerShell
mario rossi
```

## Approfondimento

Prima dell'introduzione dei metodi ToLower() e ToUpper() in PowerShell, per convertire una stringa in minuscolo o maiuscolo, i programmatori utilizzavano comandi eseguibili come tr e awk. Inoltre, le operazioni di conversione di stringhe non erano supportate in modo nativo e richiedevano librerie di terze parti o anche manipolazioni dirette dei codici ASCII.

Sebbene ToLower() sia il metodo più comune per convertire una stringa in minuscolo in PowerShell, ci sono alternative come il metodo String.ToLowerInvariant() che è consigliato quando si lavora con i dati che devono essere normalizzati e confrontati in modo affidabile.

La funzione ToLower() è implementata nel .NET Framework, che PowerShell utilizza come base. Questo metodo mappa ogni carattere nella stringa a corrispondente carattere in minuscolo, se esiste, utilizzando le regole della cultura corrente.

## Guarda Anche:

- Documentazione Microsoft su [ToLower()](https://docs.microsoft.com/it-it/dotnet/api/system.string.tolower?view=net-5.0) e [ToLowerInvariant()](https://docs.microsoft.com/it-it/dotnet/api/system.string.tolowerinvariant?view=net-5.0).
- Blog su [Come usare i metodi stringa di PowerShell](https://devblogs.microsoft.com/scripting/using-powershell-string-manipulation/).