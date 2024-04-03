---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:44.847381-07:00
description: "Hur man g\xF6r: I PowerShell kan du anv\xE4nda operatorerna `-match`,\
  \ `-replace` och `-split`, bland andra, f\xF6r att utf\xF6ra \xE5tg\xE4rder med\
  \ regulj\xE4ra uttryck. L\xE5t\u2026"
lastmod: '2024-03-13T22:44:38.114188-06:00'
model: gpt-4-0125-preview
summary: "I PowerShell kan du anv\xE4nda operatorerna `-match`, `-replace` och `-split`,\
  \ bland andra, f\xF6r att utf\xF6ra \xE5tg\xE4rder med regulj\xE4ra uttryck."
title: "Att anv\xE4nda regulj\xE4ra uttryck"
weight: 11
---

## Hur man gör:
I PowerShell kan du använda operatorerna `-match`, `-replace` och `-split`, bland andra, för att utföra åtgärder med reguljära uttryck. Låt oss utforska några exempel:

### Använda `-match` för att kontrollera om en sträng matchar ett mönster
Denna operator returnerar `$true` om mönstret hittas i strängen, och `$false` annars.

```powershell
"hello world" -match "\w+orld"
# Utdata: Sant
```

### Extrahera matchningar
Du kan extrahera det matchade värdet genom att komma åt den automatiska variabeln `$matches`.

```powershell
if ("I have 100 apples" -match "\d+") {
    "Hittat nummer: " + $matches[0]
}
# Utdata: Hittat nummer: 100
```

### Använda `-replace` för substitutioner
Operatorn `-replace` ersätter alla förekomster av ett mönster med en angiven ersättningssträng.

```powershell
"foo bar baz" -replace "ba[rz]", "qux"
# Utdata: foo qux qux
```

### Dela strängar med `-split`
Dela en sträng i en vektor av delsträngar baserat på ett regex-mönster.

```powershell
"The quick-brown_fox jumps" -split "[-_ ]"
# Utdata: The quick brown fox jumps
```

### Avancerad mönstermatchning
PowerShell stöder också mer komplexa regex-operationer via `[regex]`-klassen, vilket ger dig tillgång till metoder som `Matches()`, `Replace()`, och `Split()`.

```powershell
[regex]::Matches("June 24, August 9, Dec 12", "\b[A-Za-z]+\b").Value
# Utdata: June August Dec

[regex]::Replace("100,000", "\B(?=(?:\d{3})+(?!\d))", ",")
# Utdata: 100,000

[regex]::Split("one,two;three four", ",|;| ")
# Utdata: one two three four
```

Dessa exempel visar kraften och mångsidigheten hos reguljära uttryck i PowerShell för datamanipulation och mönstermatchning. Genom att utnyttja regex kan programmerare utföra komplex textbearbetning effektivt.
