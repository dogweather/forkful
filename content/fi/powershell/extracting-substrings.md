---
title:                "Alimerkkijonojen erottaminen"
html_title:           "PowerShell: Alimerkkijonojen erottaminen"
simple_title:         "Alimerkkijonojen erottaminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Substringien poimiminen tarkoittaa tietyn osan tekstistä ottamista, esimerkiksi merkkijonosta tai tiedostonimestä. Ohjelmoijat käyttävät tätä toimintoa, kun he haluavat käsitellä vain tietyn osan tekstistä ja jättää muut osat huomiotta.

## Miten:

Voit käyttää PowerShell-skriptejä substringien poimimiseen helposti. Käytä komentoa "Substring", jonka jälkeen annat alkuperäisen tekstin, aloitusindeksin ja lopetuspisteen. Katso alla oleva esimerkki:

```
PowerShell $teksti = "Tämä on teksti, josta haluan poimia vain toisen lauseen." Substring $teksti 18 36
```

```
Output: vain toisen lauseen
```

Voit myös käyttää "Split" komentoa poimimaan tietyt merkit tai sanat tekstin sisältä. Katso alla oleva esimerkki:

```
PowerShell $teksti = "Tämä on esimerkkiteksti." Split $teksti " on "
```

```
Output: esimerkkiteksti.
```

## Syvemmällä:

Substringien poimiminen on ollut osa ohjelmointia jo pitkään, mutta PowerShell tarjoaa helpon tavan suorittaa tämä toiminto suoraan komentoriviltä. Myös muut ohjelmointikielet, kuten C# ja JavaScript, tarjoavat vastaavia toimintoja substringsien käsittelyyn.

## Katso myös:

Voit löytää lisää tietoa substringien poimimisesta ja PowerShellin käytöstä seuraavista lähteistä:

- [Microsoftin virallinen PowerShell-dokumentaatio](https://docs.microsoft.com/fi-fi/powershell/)
- [PowerShell-tutoriaalit ja oppaat](https://www.tutorialspoint.com/powershell/)
- [PowerShell-ryhmä keskustelufoorumi](https://www.powershell.org/community/)