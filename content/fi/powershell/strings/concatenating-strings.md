---
date: 2024-01-20 17:35:17.906022-07:00
description: "Kuinka: Aikojen alussa, kun komentosarjoja ensin kirjoitettiin, merkkijonojen\
  \ yhdist\xE4minen oli keino tuottaa dynaamista sis\xE4lt\xF6\xE4. PowerShelliss\xE4\
  \u2026"
lastmod: '2024-04-05T21:53:58.347165-06:00'
model: gpt-4-1106-preview
summary: "Aikojen alussa, kun komentosarjoja ensin kirjoitettiin, merkkijonojen yhdist\xE4\
  minen oli keino tuottaa dynaamista sis\xE4lt\xF6\xE4."
title: "Merkkijonojen yhdist\xE4minen"
weight: 3
---

## Kuinka:
```PowerShell
# Yksinkertainen yhdistäminen käyttäen plus-merkkiä (+)
$tervehdys = "Hei, " + "maailma!"
Write-Output $tervehdys  # Tulostaa "Hei, maailma!"

# Muuttujien yhdistäminen ja tekstin lisääminen
$nimi = "Juuso"
$toivotus = "$tervehdys $nimi!"
Write-Output $toivotus  # Tulostaa "Hei, maailma! Juuso!"

# Yhdistäminen käyttäen -f operaattoria (format-operaattoria)
$paikka = "Suomi"
Write-Output ("Terveiset {0}stä, {1}!" -f $paikka, $nimi)  # Tulostaa "Terveiset Suomesta, Juuso!"

# Here-String käyttö monirivisen tekstin yhteydessä
$runo = @"
Tämä on runo,
joka jatkuu useammalle riville,
yhdistäen monta tekstinpätkää
yhteen.
"@
Write-Output $runo
```

## Syväsukellus
Aikojen alussa, kun komentosarjoja ensin kirjoitettiin, merkkijonojen yhdistäminen oli keino tuottaa dynaamista sisältöä. PowerShellissä yhdistämiseen voi käyttää plus-merkkiä (+), mutta myös tehokkaampia metodeja, kuten formaatti-operaattoria (-f), joka mahdollistaa monimutkaisemmatkin yhdistelyt ja tekstinmuotoilut.

Yhdistämisen vaihtoehtoina ovat myös funktiot kuten `Join-String` PowerShell v6.0+:ssa tai .NET-metodit kuten `[string]::Concat()`. Yhdistettäessä suuria määriä tai suorituskyvyn ollessa kriittistä, `[System.Text.StringBuilder]` luokka tarjoaa resurssitehokkaan tavan kasata merkkijonoja.

## Katso Myös
- `Join-String` cmdlet-ohje: [Join-String](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/join-string?view=powershell-7)
- .NET `StringBuilder` luokka: [StringBuilder Class](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=net-6.0)
