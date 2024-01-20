---
title:                "Generazione di numeri casuali"
html_title:           "Arduino: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

La generazione di numeri casuali in programmazione è un processo di creazione di sequenze numeriche che non possono essere ragionevolmente previste meglio della casualità. Gli sviluppatori utilizzano numeri casuali per simulare eventi imprevedibili, per testare le funzioni in scenari variabili e per aggiungere elemento di sorpresa.

## Come fare:

In PowerShell, la generazione di numeri casuali è un compito semplice con il cmdlet `Get-Random`. Ecco un esempio:

```PowerShell
# Genera un numero casuale tra 0 e 100
Get-Random -Maximum 100
```

Esito potrebbe essere `67`.

Per generare più numeri casuali, aggiungi il cmdlet `foreach`:

```PowerShell
# Genera 5 numeri casuali tra 0 e 100
1..5 | foreach { Get-Random -Maximum 100 }
```

L'esito potrebbe essere `43, 89, 2, 64, 23`.

## Approfondimento:

Prima dell'arrivo di PowerShell, gli sviluppatori utilizzavano funzioni come `rand()` in C/C++ o `Random` in .NET per generare numeri casuali.

Sebbene `Get-Random` riesca a fare il lavoro nella maggior parte dei casi, ci sono alternative. Ad esempio, potresti utilizzare il metodo .NET `[System.Guid]::NewGuid()` per generare un GUID univoco.

Quando generi numeri casuali in PowerShell, è importante ricordare che il cmdlet `Get-Random` utilizza un generatore di numeri pseudo-casuali. Non è adatto per generare numeri che richiedono un alto grado di sicurezza, come i numeri di carte di credito.

## Vedi Anche:

- [Documentazione ufficiale di Microsoft per Get-Random](https://docs.microsoft.com/it-it/powershell/module/microsoft.powershell.utility/get-random?view=powershell-7.2)
- [Generazione di numeri casuali in .NET](https://docs.microsoft.com/it-it/dotnet/api/system.random?view=net-6.0)
- [Guida alla generazione di numeri casuali in C++](https://en.cppreference.com/w/cpp/numeric/random)