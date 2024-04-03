---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:25.480314-07:00
description: "Hvordan: PowerShell tilbyr enkle cmdleter for \xE5 f\xE5 tak i dato\
  \ og klokkeslett. Cmdleten `Get-Date` er hovedverkt\xF8yet for dette form\xE5let.\
  \ Den kan returnere\u2026"
lastmod: '2024-03-13T22:44:41.027716-06:00'
model: gpt-4-0125-preview
summary: "PowerShell tilbyr enkle cmdleter for \xE5 f\xE5 tak i dato og klokkeslett."
title: "F\xE5 dagens dato"
weight: 29
---

## Hvordan:
PowerShell tilbyr enkle cmdleter for å få tak i dato og klokkeslett. Cmdleten `Get-Date` er hovedverktøyet for dette formålet. Den kan returnere fullstendig dato og tid, som du kan formatere eller manipulere etter dine behov.

```powershell
# Få den nåværende datoen og klokkeslettet
Get-Date
```

**Eksempelutdata:**

```
Tirsdag, september 5, 2023 9:46:02 AM
```

Du kan også formatere utdataene for å vise bare informasjonen du trenger, slik som bare datoen eller bare klokkeslettet.

```powershell
# Få bare dagens dato i et spesifikt format
Get-Date -Format "yyyy-MM-dd"
```

**Eksempelutdata:**

```
2023-09-05
```

```powershell
# Få bare det nåværende klokkeslettet
Get-Date -Format "HH:mm:ss"
```

**Eksempelutdata:**

```
09:46:02
```

### Bruke .NET-klasse
PowerShell tillater direkte tilgang til .NET-klasser, som tilbyr en alternativ måte å jobbe med datoer og klokkeslett på.

```powershell
# Bruke .NET DateTime-klassen for å få den nåværende datoen og klokkeslettet
[System.DateTime]::Now
```

**Eksempelutdata:**

```
Tirsdag, september 5, 2023 9:46:02 AM
```

For UTC-tid:

```powershell
# Bruke .NET DateTime-klassen for å få den nåværende UTC-datoen og klokkeslettet
[System.DateTime]::UtcNow
```

**Eksempelutdata:**

```
Tirsdag, september 5, 2023 1:46:02 PM
```

Disse kommandoene og klassene tilbyr kraftfulle og fleksible alternativer for å jobbe med datoer og tider i PowerShell, essensielt for mange skripting- og automatiseringsoppgaver.
