---
title:                "Få dagens dato"
date:                  2024-02-03T19:10:25.480314-07:00
model:                 gpt-4-0125-preview
simple_title:         "Få dagens dato"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva og hvorfor?

Å hente dagens dato i PowerShell handler om å hente systemets nåværende dato og tid. Denne operasjonen er fundamental for oppgaver som logging, tidsoperasjoner, eller å ta avgjørelser basert på datoer. Programmerere bruker denne muligheten til å spore hendelser, planlegge oppgaver og håndtere dato-spesifikk logikk i skript og applikasjoner.

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
