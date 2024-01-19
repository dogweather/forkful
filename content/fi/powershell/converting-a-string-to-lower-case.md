---
title:                "Merkkijonon muuttaminen pieniksi kirjaimiksi"
html_title:           "Gleam: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

Muuttaa merkkijono pienaakkosiksi tarkoittaa kaikkien isoja kirjaimia sisältävän merkkijonon muuttamista pienaakkosiksi. Tätä tarvitaan, koska ohjelmoijat usein vertailevat merkkijonoja, ja nämä vertailut ovat usein kirjainkoon herkkiä.


## Kuinka:

PowerShellissä tämä on erittäin yksinkertaista. Käytä `ToLower()` metodia. Tässä on esimerkki:

```PowerShell
$sana = "Moi MAAILMA! "
$alempi_sana = $sana.ToLower()
echo $alempi_sana

# Tulostaa: "moi maailma!"
```
Verrattuna alkuperäiseen merkkijonoon, uudessa muuttujassa kaikki kirjaimet ovat pieniä.

## Syvällisempi tarkastelu:

Merkkijonon muuttaminen pienaakkosiksi ei ole uusi käsite. Se on ollut mahdollista jo ohjelmointikielistä alkaen, kuten Perl ja JavaScript. PowerShell on sisällyttänyt `ToLower()` metodin .NET-kirjastoihin, jotka ovat olemassa ohjelmointikielestä .NET Framework 1.0, julkaistu vuonna 2002.

On vaihtoehtoinen tapa muuttaa merkkijono pienaakkosiksi PowerShellissä. Käyttää `mb.ToLower()`-metodia, joka tekee saman asian, mutta se on määritetty tekstin kulttuuriasetuksille. Se muuttaa merkkijonon pienaakkosiksi mukauttamalla merkkijonoa kulttuurille, joka on määritetty nykyiselle threadille.

## Katso myös:

- `.NET String Class`: https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-5.0 
- `PowerShell Documentation`: https://docs.microsoft.com/en-us/powershell/scripting/overview?view=powershell-7.1