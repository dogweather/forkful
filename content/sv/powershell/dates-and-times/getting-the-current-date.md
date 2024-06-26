---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:24.492951-07:00
description: "Hur: PowerShell erbjuder enkla cmdlets f\xF6r att f\xE5 datum och tid.\
  \ Cmdleten `Get-Date` \xE4r det fr\xE4msta verktyget f\xF6r detta \xE4ndam\xE5l.\
  \ Den kan returnera hela\u2026"
lastmod: '2024-03-13T22:44:38.136261-06:00'
model: gpt-4-0125-preview
summary: "PowerShell erbjuder enkla cmdlets f\xF6r att f\xE5 datum och tid."
title: "F\xE5 det aktuella datumet"
weight: 29
---

## Hur:
PowerShell erbjuder enkla cmdlets för att få datum och tid. Cmdleten `Get-Date` är det främsta verktyget för detta ändamål. Den kan returnera hela datumet och tiden, som du sedan kan formatera eller manipulera enligt dina behov.

```powershell
# Få det aktuella datumet och tiden
Get-Date
```

**Exempel på utdata:**

```
Tuesday, September 5, 2023 9:46:02 AM
```

Du kan också formatera utmatningen för att bara visa den information du behöver, såsom bara datumet eller bara tiden.

```powershell
# Få bara det aktuella datumet i ett specifikt format
Get-Date -Format "yyyy-MM-dd"
```

**Exempel på utdata:**

```
2023-09-05
```

```powershell
# Få bara den aktuella tiden
Get-Date -Format "HH:mm:ss"
```

**Exempel på utdata:**

```
09:46:02
```

### Använda .NET-klassen
PowerShell tillåter direkt åtkomst till .NET-klasser, vilket erbjuder ett alternativt sätt att arbeta med datum och tider.

```powershell
# Använda .NET DateTime-klassen för att få det aktuella datumet och tiden
[System.DateTime]::Now
```

**Exempel på utdata:**

```
Tuesday, September 5, 2023 9:46:02 AM
```

För UTC-tid:

```powershell
# Använda .NET DateTime-klassen för att få det aktuella UTC-datumet och tiden
[System.DateTime]::UtcNow
```

**Exempel på utdata:**

```
Tuesday, September 5, 2023 1:46:02 PM
```

Dessa kommandon och klasser ger kraftfulla och flexibla alternativ för att arbeta med datum och tider i PowerShell, vilket är nödvändigt för många skript- och automatiseringsuppgifter.
