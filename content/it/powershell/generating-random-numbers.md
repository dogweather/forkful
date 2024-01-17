---
title:                "Generare numeri casuali"
html_title:           "PowerShell: Generare numeri casuali"
simple_title:         "Generare numeri casuali"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Generare numeri casuali è un modo per ottenere un valore casuale o imprevedibile su richiesta. I programmatori spesso lo fanno per creare giochi o simularazioni, o per testare l'affidabilità di un algoritmo in diversi scenari.

## Come farlo:
```PowerShell
# Esempio di generazione di un numero casuale tra 1 e 10
Get-Random -Minimum 1 -Maximum 10

# Esempio di generazione di un numero casuale tra -50 e 50
Get-Random -Minimum -50 -Maximum 50

# Esempio di generazione di una stringa casuale di 10 caratteri
[char[]]([char]'a'..[char]'z' + [char]'A'..[char]'Z' + [char]'0'..[char]'9' + [char]'$' + [char]'&') | Get-Random -Count 10 -Join ""
```

**Output:**
- 5
- 24
- yUn9j0Clm9

## Approfondimento:
La generazione di numeri casuali viene spesso utilizzata nelle applicazioni di gioco e nei simulatori, ma anche in molti altri contesti come la crittografia e la sicurezza informatica. In passato, i programmatori hanno dovuto sviluppare i propri algoritmi per generare numeri casuali, ma oggi è possibile utilizzare gli strumenti forniti da PowerShell, come il cmdlet Get-Random. Alcune alternative per la generazione di numeri casuali includono l'utilizzo di librerie esterne o l'utilizzo di funzioni hash.

## Vedi anche:
- [Documentazione Microsoft su cmdlet Get-Random](https://docs.microsoft.com/it-it/powershell/module/microsoft.powershell.utility/get-random?view=powershell-7.1)
- [Funzione hash in PowerShell](https://docs.microsoft.com/it-it/powershell/module/microsoft.powershell.security/get-hashalgorithm?view=powershell-7.1)
- [Libreria esterna di generazione di numeri casuali per PowerShell](https://github.com/rencinrig/PSRandom)