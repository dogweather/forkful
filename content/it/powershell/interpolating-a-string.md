---
title:                "Interpolazione di una stringa"
html_title:           "Clojure: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è & Perché?

L'interpolazione di stringhe è un metodo per infondere variabili all'interno di una stringa. I programmatori lo fanno per comporre messaggi dinamici e renderne più leggibile il codice.

## Come fare:

Ecco come si fa in PowerShell. Supponiamo di avere due variabili, `$nome` e `$età`.

```PowerShell
$nome = "Mario"
$età = 25
```

Per interpolare queste variabili in una stringa, usiamo `"..."` (doppio apice) e inseriamo le variabili con `$`:

```PowerShell
"Buongiorno, mio nome è $nome e ho $età anni."
```

L'output sarà:

```PowerShell
Buongiorno, mio nome è Mario e ho 25 anni.
```

## Approfondimento

L'interpolazione di stringhe non è un concetto nuovo. È presente in molti altri linguaggi di programmazione come JavaScript e Python. In PowerShell, noterai che solo le stringhe racchiuse tra doppi apici `"..."` possono essere interpolate. Le stringhe racchiuse in apici singoli `'...'` non faranno interpolazione.

Come alternativa all'interpolazione di stringhe in PowerShell, potresti usare il metodo tradizionale `Format-String`:

```PowerShell
"Buongiorno, mio nome è {0} e ho {1} anni." -f $nome, $età
```

Tuttavia, l'interpolazione di stringhe è generalmente considerata più leggibile.

## Vedere Anche

2. [Documentazione di Microsoft su `-f` operator](https://docs.microsoft.com/it-it/powershell/module/microsoft.powershell.core/about/about_operators?view=powershell-7.1#format-operator-f)