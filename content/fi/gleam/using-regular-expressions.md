---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Gleam: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?
Regular expressionien käyttö on tapa etsiä ja muokata tekstejä käyttämällä erityisiä merkkijonoja ja sääntöjä, joita kutsutaan lausekkeiksi. Tämä on hyödyllistä ohjelmoijille, jotka haluavat hakea ja muokata tiettyjä tietoja tekstidatasta nopeasti ja tarkasti.

# Miten?
Gleam sisältää vakio-kirjaston regular expressionien käyttöön. Alla olevassa esimerkissä näytämme, kuinka voit käyttää regular expressioneja Gleamin kanssa.

```
Gleam.String.split("Tämä on esimerkki!", ~pattern="\\s")
|> Debug.todo
```

Tässä esimerkissä käytämme `split`-funktiota jakamaan tekstin, joka perustuu sääntöön "\s", eli kaikki sanat erotettuna välilyönneillä. Tämän jälkeen käytämme `Debug.todo`-funktiota tulostamaan jakautuneet sanat.

```
Tämä |> on |> esimerkki!
```

Tulostettu tulos olisi:

```
["Tämä", "on", "esimerkki!"]
```

# Syventymistä
Regular expressionit ovat olleet osa ohjelmointia jo vuosikymmeniä ja niitä käytetään edelleen paljon eri kielissä. Vaikka Gleamin valmiiksi määritetty regular expression-kirjasto on hyvä, on olemassa myös muita vaihtoehtoja, kuten PCRE ja POSIX. Myös eri käyttöjärjestelmät voivat tarjota omia versioitaan regular expressioneista, joilla voi olla pieniä eroja syntaxissa.

# Katso myös
- [PCRE - Regular expression library](https://www.pcre.org/)
- [POSIX - Portable Operating System Interface](https://en.wikipedia.org/wiki/POSIX)