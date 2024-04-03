---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:11.211977-07:00
description: "Merkkijonon alkukirjaimen muuttaminen isoksi kirjaimeksi PowerShelliss\xE4\
  \ tarkoittaa annetun merkkijonon ensimm\xE4isen merkin muuttamista isoksi kirjaimeksi,\u2026"
lastmod: '2024-03-13T22:44:56.763990-06:00'
model: gpt-4-0125-preview
summary: "Merkkijonon alkukirjaimen muuttaminen isoksi kirjaimeksi PowerShelliss\xE4\
  \ tarkoittaa annetun merkkijonon ensimm\xE4isen merkin muuttamista isoksi kirjaimeksi,\
  \ samalla kun loput merkkijonosta j\xE4tet\xE4\xE4n muuttumattomiksi."
title: Merkkijonon muuttaminen isoiksi kirjaimiksi
weight: 2
---

## Miten:
PowerShell, ollessaan monipuolinen työkalu, mahdollistaa merkkijonon alkukirjaimen isoksi muuttamisen yksinkertaisilla menetelmillä ilman, että tarvitsee kolmannen osapuolen kirjastoja. Näin voit tehdä sen:

```powershell
# Käyttämällä .Net-metodia 'ToTitleCase' luokasta CultureInfo
$text = "hello world"
$culture = [System.Globalization.CultureInfo]::InvariantCulture
$capitalizedText = $culture.TextInfo.ToTitleCase($text.ToLower())
Write-Output $capitalizedText
```
Tuloste:
```
Hello world
```

Huomaa: Tämä menetelmä muuttaa jokaisen sanan ensimmäisen kirjaimen isoksi kirjaimeksi. Jos haluat tiukasti muuttaa vain merkkijonon ensimmäisen kirjaimen isoksi ja jättää loput sellaisiksi kuin ne ovat, voisit tehdä jotain tällaista:

```powershell
# Muuttaen vain merkkijonon ensimmäisen merkin isoksi kirjaimeksi
$text = "hello world"
$capitalizedText = $text.Substring(0,1).ToUpper() + $text.Substring(1)
Write-Output $capitalizedText
```
Tuloste:
```
Hello world
```

PowerShell ei suoraan sisällä yksinkertaista funktiota vain merkkijonon ensimmäisen kirjaimen isoksi muuttamiseen, mutta yhdistämällä perus merkkijonojen käsittelymenetelmiä, kuten `Substring(0,1).ToUpper()` ja yhdistämistä, voimme helposti saavuttaa halutun tuloksen.
