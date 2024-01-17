---
title:                "Joukon merkkien interpolointi"
html_title:           "PowerShell: Joukon merkkien interpolointi"
simple_title:         "Joukon merkkien interpolointi"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

Merkkijonon interpolaatio on tekniikka, jossa merkkijonojen muotoilusääntöjä käytetään merkkijonon sisällön lisäämiseen toiseen merkkijonoon. Tätä käytetään usein ohjelmoinnissa, kun halutaan yhdistää dynaamisia tai muuttuvia arvoja staattisiin teksteihin. Esimerkiksi voimme haluta tulostaa jonkin laskennallisen arvon välilyönnillä ja tekstillä varustettuna.

# Miten:

Interpolaatio voidaan toteuttaa PowerShell:ssa kahdella tavalla: joko käyttämällä jo valmiiksi muodostettua merkkijonoa tai luomalla se kokonaan uudestaan.

```PowerShell
# Muodostettu merkkijono
$num = 5
Write-Host "Tulos on: $num" // Tulostaa "Tulos on: 5"

# Luotu merkkijono
Write-Host "Tulos on: $($num + 2)" // Tulostaa "Tulos on: 7"
```

# Syväsukellus:

Merkkijonon interpolaatio ei ole uusi tekniikka, vaan se on saanut inspiraatiota jo varhaisista ohjelmointikielistä kuten PL/I ja Algol. Nykyään sitä käytetään laajasti eri ohjelmointikielillä, kuten C#, Java ja tietenkin PowerShell. Vaihtoehtoisesti interpolaation sijaan voimme käyttää myös merkkijonon formatointia tai konkatinointia saavuttaaksemme saman tuloksen.

# Katso myös:

- https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_operators?view=powershell-7 - Lisätietoa operaattoreista PowerShell:ssa.
- https://stackoverflow.com/questions/43353680/what-is-interpolating-a-string - Aiheeseen liittyvä keskustelu Stack Overflow:ssa.
- https://www.w3schools.com/python/gloss_python_string_interpolation.asp - Interpolaation toteutus Pythonissa.