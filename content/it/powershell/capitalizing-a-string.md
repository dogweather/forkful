---
title:                "Maiuscolizzare una stringa"
date:                  2024-01-19
html_title:           "Bash: Maiuscolizzare una stringa"
simple_title:         "Maiuscolizzare una stringa"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizzare una stringa significa trasformare tutte le sue lettere in maiuscole. Lo facciamo per conformità, evidenza o per rispettare certi protocolli di formattazione.

## How to:
Utilizza il metodo `.ToUpper()` per capitalizzare una stringa. Ecco un esempio semplice:

```PowerShell
$stringa = "ciao mondo"
$stringaMaiuscola = $stringa.ToUpper()
$stringaMaiuscola
```

Output:
```
CIAO MONDO
```

E se vuoi solo la prima lettera di ogni parola in maiuscolo:

```PowerShell
$stringa = "buongiorno italia"
$stringaTitolo = $stringa.ToTitleCase()
$stringaTitolo
```
Occhio: `ToTitleCase()` non è direttamente disponibile. 

Usa `[CultureInfo]` per prima cosa:
```PowerShell
$culture = [System.Globalization.CultureInfo]::CurrentCulture
$textInfo = $culture.TextInfo
$stringaTitolo = $textInfo.ToTitleCase($stringa)
$stringaTitolo
```

Output:
```
Buongiorno Italia
```

## Deep Dive:
Capitalizzare una stringa è basilare ma importante. Nasce dall’esigenza di uniformare l'input per confronti o per estetica. In PowerShell, `.ToUpper()` è intuitivo e diretto. Storicamente, il linguaggio ha sempre fornito modi per manipolare stringhe, riflettendo l'importanza di questo compito.

Ci sono alternative: puoi usare metodi nativi come `.ToUpperInvariant()` per ignorare le specificità culturali durante la capitalizzazione, utile per assicurare consistenza in contesti internazionali.

Su come funzionano: chiamare questi metodi su una stringa genera una nuova stringa con le modifiche desiderate. PowerShell, come .NET, tratta le stringhe come immutabili. Questo significa che non puoi cambiare una stringa esistente, ma ne crei una nuova.

## See Also:
- [Cosa significa immutabile?](https://docs.microsoft.com/it-it/dotnet/csharp/programming-guide/strings/)
- [Documentazione .NET su String.ToUpper](https://docs.microsoft.com/it-it/dotnet/api/system.string.toupper?view=net-7.0)
- [Documentazione .NET su CultureInfo](https://docs.microsoft.com/it-it/dotnet/api/system.globalization.cultureinfo?view=net-7.0)
