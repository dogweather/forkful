---
title:                "Gleam: Tekstin etsiminen ja korvaaminen"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Miksi etsiä ja korvata tekstiä?

Etsi ja korvaa tekstiä -toiminto on tärkeä osa jokapäiväistä ohjelmointia. Se auttaa sinua nopeasti ja tehokkaasti korvaamaan tekstin tietystä tiedostosta tai koodista. Tämä säästää aikaa ja vaivaa manuaaliselta etsimiseltä ja korvaamiselta.

# Miten se tehdään?

Voit hyödyntää Gleam-ohjelmointikielen tarjoamaa helppoa ja tehokasta tekstinsyöttötoimintoa.

```Gleam
// Luo uusi muuttuja nimeltä "teksti" ja aseta siihen alkuarvo
teksti = "Tämä on alkuperäinen teksti"

// Etsi tekstistä sana "alkuperäinen" ja korvaa se sanalla "uusi"
uusi_teksti = teksti |> String.replace("alkuperäinen", "uusi")

// Tulostaa uuden tekstin
IO.print(uusi_teksti)
``` 

```
Tämä on uusi teksti
```

Gleam tarjoaa myös muita vaihtoehtoja haun ja korvauksen muuttamiseen, kuten säännölliset lausekkeet ja erilaiset vertailukriteerit. Voit löytää kaikki Gleam-tekstinmuokkaustoiminnot Gleamin virallisesta dokumentaatiosta.

# Syvemmälle aiheeseen

Etsi ja korvaa -toiminnossa on useita hienosäätömahdollisuuksia, kuten haun tarkentaminen tiettyihin osiin tekstistä ja erilaisten korvausten yhdistäminen toisiinsa. Lisäksi voit myös suorittaa tekstikäsittelytoimintoja useassa tiedostossa kerrallaan.

# Katso myös

- [Gleam-ohjelmointikielen viralliset sivut] (https://gleam.run/)
- [Gleamin tekstinsyöttötoiminnon dokumentaatio] (https://gleam.run/documentation)
- [Gleamin tekstinmuokkaustoimintojen esimerkkejä] (https://github.com/gleam-lang/gleam/blob/master/examples/strings)