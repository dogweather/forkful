---
title:                "Capitalizzare una stringa"
html_title:           "PowerShell: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

---
# Capitalizza una Stringa in PowerShell: Un Semplice Tutorial

## Perché e Perché?

Capitalizzare una stringa significa trasformare la prima lettera di ogni parola in un carattere maiuscolo. Programmatori lo fanno per migliorare la leggibilità e la presentazione dei dati.

## Come fare:

Usiamo la funzione `ToUpper()`, per capitalizzare le stringhe in PowerShell. Ecco un esempio:

```PowerShell
$stringa = "ciao mondo"
$stringa = $stringa.Split(' ').ForEach({$_.Substring(0,1).ToUpper()+$_.Substring(1)})
$stringa -join ' '
```

Quando si esegue il codice sopra, il risultato sarà:

```PowerShell
"Ciao Mondo"
```

## Approfondimenti:

Nel tempo, molti metodi sono stati sviluppati per capitalizzare stringhe. In passato, programmatori spesso usavano `TextInfo.ToTitleCase()`, ma questa funzione non cambia i caratteri maiuscoli esistenti in minuscoli. Ad esempio, `TextInfo.ToTitleCase("CIAO")` restituirebbe `CIAO`, non `Ciao`.

Una scelta moderna potrebbe essere usare i metodi `ToLower()` o `ToUpper()`, insieme alla manipolazione delle stringhe, come nell'esempio sopra. In realtà, c'è una grande varietà di metodi che potresti scegliere basati sulla tua situazione specifica.

Per quanto riguarda i dettagli di implementazione, `ToUpper()` e `Substring()` sono metodi forniti dalla classe `String` di .NET. Questo significa che sono molto efficienti, poiché sono implementati a un livello molto basso.

## Vedi anche:

1. [Microsoft Docs: Metodi String](https://docs.microsoft.com/it-it/dotnet/api/system.string?view=netframework-4.8)
2. [PowerShell: Manipolazione delle stringhe](https://powertheshell.com/powershell-string-manipulation/)
3. [Microsoft Docs: Metodi TextInfo](https://docs.microsoft.com/it-it/dotnet/api/system.globalization.textinfo?view=net-5.0)