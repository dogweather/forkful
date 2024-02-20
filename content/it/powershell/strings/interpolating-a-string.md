---
date: 2024-01-20 17:51:22.254741-07:00
description: "L'interpolazione di una stringa consente di inserire variabili o espressioni\
  \ direttamente all'interno di una stringa di testo. I programmatori la usano\u2026"
lastmod: 2024-02-19 22:05:02.700884
model: gpt-4-1106-preview
summary: "L'interpolazione di una stringa consente di inserire variabili o espressioni\
  \ direttamente all'interno di una stringa di testo. I programmatori la usano\u2026"
title: Interpolazione di una stringa
---

{{< edit_this_page >}}

## What & Why?
L'interpolazione di una stringa consente di inserire variabili o espressioni direttamente all'interno di una stringa di testo. I programmatori la usano per costruire stringhe in modo dinamico e leggibile, fondamentale per l'output personalizzato e la formattazione dei dati.

## How to:
Interpolare una stringa in PowerShell è semplice: usate il doppio apice `" "` e inserite variabili con il simbolo del dollaro `$` seguito dal nome della variabile. Per esempio:

```PowerShell
$nome = "Luca"
$età = 28
$saluto = "Ciao, mi chiamo $nome e ho $età anni."
Write-Output $saluto
```
Risultato:
```
Ciao, mi chiamo Luca e ho 28 anni.
```

Potete anche usare espressioni all'interno dell'interpolazione, tipo così:

```PowerShell
$ora = Get-Date
Write-Output "Ora esatta: $($ora.ToString('HH:mm:ss'))"
```
Risultato:
```
Ora esatta: 14:35:07
```

## Deep Dive
In PowerShell, l'interpolazione di stringhe è stata introdotta con la versione 2.0. È un bel miglioramento rispetto all'antico metodo di concatenazione di stringhe, che rendeva il codice più macchinoso e difficile da leggere. 

Ad esempio, senza interpolazione, il codice di sopra sarebbe stato:

```PowerShell
$nome = "Luca"
$età = 28
$saluto = "Ciao, mi chiamo " + $nome + " e ho " + $età.ToString() + " anni."
Write-Output $saluto
```

Menomale che ora possiamo semplificare, eh?

Oltre all'interpolazione, potete sempre utilizzare il formato stringa, specialmente quando volete più controllo sulla formattazione, tramite l'operatore `-f`:

```PowerShell
$nome = "Luca"
$età = 28
$saluto = "Ciao, mi chiamo {0} e ho {1} anni." -f $nome, $età
Write-Output $saluto
```

## See Also
Per approfondire l'interpolazione di stringhe e le altre tecniche di formattazione in PowerShell, ecco qualche link utile:

- [About Quoting Rules](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_quoting_rules)
- [PowerShell String Format - How to Use Format Operator in PS](https://adamtheautomator.com/powershell-string-format/)
