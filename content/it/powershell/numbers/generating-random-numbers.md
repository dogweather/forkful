---
date: 2024-01-27 20:35:01.543051-07:00
description: "Generare numeri casuali in PowerShell riguarda la creazione di valori\
  \ numerici imprevedibili entro un intervallo specificato. I programmatori utilizzano\u2026"
lastmod: '2024-03-13T22:44:43.636960-06:00'
model: gpt-4-0125-preview
summary: Generare numeri casuali in PowerShell riguarda la creazione di valori numerici
  imprevedibili entro un intervallo specificato.
title: Generazione di numeri casuali
weight: 12
---

## Come fare:
PowerShell offre un approccio diretto per generare numeri casuali usando il cmdlet `Get-Random`. Questo cmdlet può produrre numeri casuali entro un intervallo predefinito o specificato.

```PowerShell
# Genera un numero casuale tra 0 e Int32.MaxValue
$randomNumber = Get-Random
Write-Output $randomNumber
```

Per specificare un intervallo, usare i parametri `-Minimum` e `-Maximum`:

```PowerShell
# Genera un numero casuale tra 1 e 100
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Output $randomNumber
```

Per avere maggiore controllo, è possibile istanziare un oggetto della classe `System.Random`:

```PowerShell
# Usando System.Random per una sequenza di numeri
$rand = New-Object System.Random
foreach ($i in 1..5) {
    $randomNumber = $rand.Next(1, 101)
    Write-Output $randomNumber
}
```

Se hai bisogno di una selezione casuale da un array o una raccolta, `Get-Random` può scegliere direttamente un elemento:

```PowerShell
# Selezione casuale da un array
$array = 1..10
$randomItem = Get-Random -InputObject $array
Write-Output $randomItem
```

## Approfondimento
Il cmdlet `Get-Random` in PowerShell sfrutta sotto il cofano la classe .NET `System.Random` per generare numeri pseudocasuali. Questi sono "pseudo" perché utilizzano algoritmi per produrre sequenze di numeri che sembrano solo casuali. Per la maggior parte delle applicazioni, questo livello di casualità è sufficiente. Tuttavia, per casi d'uso che richiedono sicurezza crittografica, `System.Random` non è adatto a causa della sua natura prevedibile.

PowerShell e .NET offrono `System.Security.Cryptography.RNGCryptoServiceProvider` per la casualità crittografica, che è più appropriato per generare chiavi di crittografia o altre operazioni sensibili alla sicurezza:

```PowerShell
# Numeri casuali crittograficamente sicuri
$rng = [System.Security.Cryptography.RNGCryptoServiceProvider]::new()
$bytes = New-Object byte[] 4
$rng.GetBytes($bytes)
$randomNumber = [BitConverter]::ToInt32($bytes, 0)
Write-Output $randomNumber
```

Mentre `Get-Random` e `System.Random` soddisfano un ampio insieme di esigenze per la casualità nello scripting e nella logica applicativa, è essenziale selezionare lo strumento giusto per il lavoro, specialmente in applicazioni incentrate sulla sicurezza dove la prevedibilità può presentare una vulnerabilità.
