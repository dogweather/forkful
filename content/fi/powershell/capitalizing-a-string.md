---
title:                "Merkkijonon kirjainten muuttaminen isoiksi"
html_title:           "PowerShell: Merkkijonon kirjainten muuttaminen isoiksi"
simple_title:         "Merkkijonon kirjainten muuttaminen isoiksi"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

-------------------

## Miksi & Mitä Varten?

Merkkijonon pääkaupungistaminen on prosessi, jossa merkkijonon ensimmäinen kirjain muutetaan suureksi kirjaimeksi. Ohjelmoijat tekevät sen parantaakseen tekstin ulkonäköä tai muodostaakseen oikeanlaisia tunnisteita.

## Kuinka Tehdään:

PowerShellissa voit käyttää `.ToUpper()`-funktiota muuttamaan koko merkkijonon isoiksi kiraimiksi. Mutta jos haluat tehdä vain ensimmäisen kirjaimen isoksi, voit tehdä sen seuraavasti:

```PowerShell
$string = "powershell"
$capitalizedString = $string.Substring(0,1).ToUpper()+$string.Substring(1)
Write-Output $capitalizedString
```

Kun suoritat tämän koodin, saat seuraavan tulostuksen:

```PowerShell
PowerShell
```

## Syvemmälle:

Merkkijonon isoilla alkukirjaimilla on pitkä ohjelmoinnin historia ja niitä on käytetty kielen standardeissa sekä ohjelman nimeämisessä. Historiallisesti tällainen muotoilu lisää lueteltavuutta ja selkeyttä.

Vaihtoehtoisesti, voit käyttää String-kirjaston `TextInfo.ToTitleCase()`-metodia, joka muuttaa koko merkkijonon ensimmäiset kirjaimet isoiksi:

```PowerShell
$textInfo = (Get-Culture).TextInfo
$string = "tämä on merkkijono"
$capitalizedString = $textInfo.ToTitleCase($string)
Write-Output $capitalizedString
```

Tämä antaa tulokseksi:

```PowerShell
Tämä On Merkkijono
```

Kuitenkin tätä menetelmää varoen, sillä se käsittelee merkkijonon kunkin sanan ensimmäisen kirjaimen, ei vain merkkijonon ensimmäistä kirjainta.

## Katso Myös:

Voit saadana lisätietoa merkkijonojen manipuloinnista PowerShellissa seuraavilla linkeillä:

2. [Microsoft Docs: TextInfo.ToTitleCase](https://docs.microsoft.com/fi-fi/dotnet/api/system.globalization.textinfo.totitlecase?view=net-5.0)

-------------------