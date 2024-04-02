---
date: 2024-01-27 20:35:01.804144-07:00
description: "Det \xE5 generere tilfeldige tall i PowerShell handler om \xE5 skape\
  \ uforutsigbare numeriske verdier innenfor et spesifisert omr\xE5de. Programmerere\
  \ bruker denne\u2026"
lastmod: '2024-03-13T22:44:41.012051-06:00'
model: gpt-4-0125-preview
summary: "Det \xE5 generere tilfeldige tall i PowerShell handler om \xE5 skape uforutsigbare\
  \ numeriske verdier innenfor et spesifisert omr\xE5de. Programmerere bruker denne\u2026"
title: Generering av tilfeldige tall
weight: 12
---

## Hva & Hvorfor?
Det å generere tilfeldige tall i PowerShell handler om å skape uforutsigbare numeriske verdier innenfor et spesifisert område. Programmerere bruker denne funksjonen av mange grunner, inkludert testing, simulering og sikkerhetsformål, der uforutsigbarhet eller etterligning av virkelighetens tilfeldighet er avgjørende.

## Hvordan:
PowerShell tilbyr en enkel tilnærming for å generere tilfeldige tall ved bruk av `Get-Random` cmdleten. Denne cmdleten kan produsere tilfeldige tall innenfor et standardområde eller et spesifisert område.

```PowerShell
# Generer et tilfeldig tall mellom 0 og Int32.MaxValue
$randomNumber = Get-Random
Write-Output $randomNumber
```

For å spesifisere et område, bruk `-Minimum` og `-Maximum` parameterne:

```PowerShell
# Generer et tilfeldig tall mellom 1 og 100
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Output $randomNumber
```

For mer kontroll, kan du instansiere et objekt av `System.Random` klassen:

```PowerShell
# Bruk System.Random for en sekvens av tall
$rand = New-Object System.Random
foreach ($i in 1..5) {
    $randomNumber = $rand.Next(1, 101)
    Write-Output $randomNumber
}
```

Hvis du trenger et tilfeldig utvalg fra en tabell eller samling, kan `Get-Random` direkte velge et element:

```PowerShell
# Tilfeldig utvalg fra en tabell
$array = 1..10
$randomItem = Get-Random -InputObject $array
Write-Output $randomItem
```

## Fordypning
`Get-Random` cmdleten i PowerShell bruker .NET-klassen `System.Random` under panseret for å generere pseudotilfeldige tall. Disse er "pseudo" fordi de bruker algoritmer til å produsere sekvenser av tall som bare ser tilfeldige ut. For de fleste applikasjoner er dette nivået av tilfeldighet tilstrekkelig. Imidlertid, for brukstilfeller som krever kryptografisk sikkerhet, er ikke `System.Random` passende på grunn av sin forutsigbare natur.

PowerShell og .NET tilbyr `System.Security.Cryptography.RNGCryptoServiceProvider` for kryptografisk tilfeldighet, som er mer passende for å generere krypteringsnøkler eller andre sikkerhetsfølsomme operasjoner:

```PowerShell
# Kryptografisk sikre tilfeldige tall
$rng = [System.Security.Cryptography.RNGCryptoServiceProvider]::new()
$bytes = New-Object byte[] 4
$rng.GetBytes($bytes)
$randomNumber = [BitConverter]::ToInt32($bytes, 0)
Write-Output $randomNumber
```

Mens `Get-Random` og `System.Random` tilfredsstiller et bredt sett med behov for tilfeldighet i scripting og applikasjonslogikk, er det essensielt å velge riktig verktøy for jobben, spesielt i sikkerhetsfokuserte applikasjoner der forutsigbarhet kan presentere en sårbarhet.
