---
title:                "Merkkijonojen yhdistäminen"
date:                  2024-01-20T17:35:17.906022-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Yhdistämme merkkijonoja luodaksemme uusia, kokonaisia tekstejä. Tämä on tarpeen, kun keräämme eri lähteistä tulevia tietoja tai haluamme muodostaa dynaamisia viestejä.

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
