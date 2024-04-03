---
date: 2024-01-20 17:35:17.906022-07:00
description: "Yhdist\xE4mme merkkijonoja luodaksemme uusia, kokonaisia tekstej\xE4\
  . T\xE4m\xE4 on tarpeen, kun ker\xE4\xE4mme eri l\xE4hteist\xE4 tulevia tietoja\
  \ tai haluamme muodostaa\u2026"
lastmod: '2024-03-13T22:44:56.772296-06:00'
model: gpt-4-1106-preview
summary: "Yhdist\xE4mme merkkijonoja luodaksemme uusia, kokonaisia tekstej\xE4."
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
