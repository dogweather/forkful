---
title:                "Skriving til standardfeil"
aliases:
- no/powershell/writing-to-standard-error.md
date:                  2024-02-03T19:34:21.346738-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skriving til standardfeil"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å skrive til standardfeil (stderr) i PowerShell innebærer å sende feilmeldinger eller diagnostikk direkte til stderr-strømmen, forskjellig fra standardutdata-strømmen (stdout). Denne separasjonen tillater mer presis kontroll over et skripts utdata, og muliggjør for utviklere å dirigere normale og feilmeldinger til forskjellige destinasjoner, noe som er grunnleggende for feilhåndtering og logging.

## Hvordan:

PowerShell forenkler prosessen med å skrive til stderr gjennom bruk av `Write-Error` cmdlet eller ved å dirigere utdata til `$host.ui.WriteErrorLine()` metoden. Men, for direkte stderr-omdirigering, kan du foretrekke å bruke .NET metoder eller fildeskriptoromdirigering tilbudt av PowerShell selv.

**Eksempel 1:** Bruke `Write-Error` for å skrive en feilmelding til stderr.

```powershell
Write-Error "Dette er en feilmelding."
```

Utdata til stderr:
```
Write-Error: Dette er en feilmelding.
```

**Eksempel 2:** Bruke `$host.ui.WriteErrorLine()` for direkte skriving til stderr.

```powershell
$host.ui.WriteErrorLine("Direkte skriving til stderr.")
```

Utdata til stderr:
```
Direkte skriving til stderr.
```

**Eksempel 3:** Bruke .NET metoder for å skrive til stderr.

```powershell
[Console]::Error.WriteLine("Bruker .NET metode for stderr")
```

Denne metodens utdata:
```
Bruker .NET metode for stderr
```

**Eksempel 4:** Omdirigere feilutdata ved bruk av fildeskriptor `2>`.

Fildeskriptorer i PowerShell kan omdirigere forskjellige strømmer. For stderr, er fildeskriptoren `2`. Her er et eksempel på omdirigering av stderr til en fil kalt `error.log` mens man utfører en kommando som genererer en feil.

```powershell
Get-Item NonExistentFile.txt 2> error.log
```

Dette eksempelet produserer ikke konsollutdata, men genererer en fil `error.log` i gjeldende katalog som inneholder feilmeldingen fra forsøket på å få tilgang til en fil som ikke eksisterer.

Som konklusjon tilbyr PowerShell flere metoder for effektivt å skrive og håndtere feilutdata, noe som tillater sofistikert feilhåndtering og loggingsstrategier i skript og applikasjoner.
