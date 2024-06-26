---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:50.152616-07:00
description: "Kuinka: PowerShell tarjoaa suoraviivaisia cmdlet-komentoja p\xE4iv\xE4\
  m\xE4\xE4r\xE4n ja ajan saamiseksi. `Get-Date` cmdlet-komento on ensisijainen ty\xF6\
  kalu t\xE4h\xE4n\u2026"
lastmod: '2024-03-13T22:44:56.792912-06:00'
model: gpt-4-0125-preview
summary: "PowerShell tarjoaa suoraviivaisia cmdlet-komentoja p\xE4iv\xE4m\xE4\xE4\
  r\xE4n ja ajan saamiseksi."
title: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hankkiminen"
weight: 29
---

## Kuinka:
PowerShell tarjoaa suoraviivaisia cmdlet-komentoja päivämäärän ja ajan saamiseksi. `Get-Date` cmdlet-komento on ensisijainen työkalu tähän tarkoitukseen. Se voi palauttaa täydellisen päivämäärän ja ajan, jonka voit muotoilla tai manipuloida tarpeidesi mukaan.

```powershell
# Hae nykyinen päivämäärä ja aika
Get-Date
```

**Esimerkkituloste:**

```
tiistai, syyskuu 5, 2023 9:46:02 AM
```

Voit myös muotoilla tulosteen näyttämään vain tarvitsemasi tiedot, kuten vain päivämäärän tai vain ajan.

```powershell
# Hae vain nykyinen päivämäärä tietyssä muodossa
Get-Date -Format "yyyy-MM-dd"
```

**Esimerkkituloste:**

```
2023-09-05
```

```powershell
# Hae vain nykyinen aika
Get-Date -Format "HH:mm:ss"
```

**Esimerkkituloste:**

```
09:46:02
```

### Käyttäen .NET-luokkaa
PowerShell sallii suoran pääsyn .NET-luokkiin, tarjoten vaihtoehtoisen tavan työskennellä päivämäärien ja aikojen kanssa.

```powershell
# Käyttäen .NET DateTime-luokkaa nykyisen päivämäärän ja ajan saamiseksi
[System.DateTime]::Now
```

**Esimerkkituloste:**

```
tiistai, syyskuu 5, 2023 9:46:02 AM
```

UTC-ajalle:

```powershell
# Käyttäen .NET DateTime-luokkaa nykyisen UTC-päivämäärän ja ajan saamiseksi
[System.DateTime]::UtcNow
```

**Esimerkkituloste:**

```
tiistai, syyskuu 5, 2023 1:46:02 PM
```

Nämä komennot ja luokat tarjoavat tehokkaita ja joustavia vaihtoehtoja työskennellä päivämäärien ja aikojen kanssa PowerShellissä, jotka ovat olennaisia monissa skriptauksessa ja automatisoinnissa.
