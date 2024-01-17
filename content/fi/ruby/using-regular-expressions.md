---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Ruby: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

Regular expressions (säännölliset lausekkeet tai "regex") ovat työkalu, jonka avulla ohjelmoijat voivat etsiä ja manipuloida tekstiä tiettyjen sääntöjen avulla. Tämä tekee niistä erittäin hyödyllisiä, kun halutaan löytää tiettyjä merkkijonoja tai muotoja tekstimuotoisista tiedoista.

# Kuinka tehdä:

Esimerkiksi, jos haluat etsiä kaikki sähköpostiosoitteet tietystä tekstidokumentista, voit käyttää seuraavaa koodia:

```
/\b[\w\.-]+@[\w\.-]+\.\w{2,4}\b/
```

Tämä etsii kaikki merkkijonot, jotka vastaavat tyypillistä sähköpostiosoitteen muotoa, kuten "esimerkki@example.com".

# Syvä sukellus:

Regular expressions syntyi 1950-luvulla matematiikan ja teoreettisen tietojenkäsittelyn yhteydessä ja ovat nyt yleisesti käytössä ohjelmoinnissa. Vaihtoehtoisia tapoja manipuloida ja etsiä tekstiä ovat esimerkiksi string-metodit Rubyssa, mutta regular expressions tarjoavat monipuolisemman tavan käsitellä erilaisia stringeja.

Regular expressions käyttää erityisiä merkintätapoja, joita kutsutaan metakaraktereiksi, muodostaakseen säännöllisiä lausekkeita. Näihin kuuluu esimerkiksi sulkumerkkejä, pistettä ja hakasulkeita, joilla määritellään tiettyjä merkkijonoja.

# Katso myös:

Jos haluat oppia lisää regular expressionsin käytöstä, voit tutustua Ruby-ohjelmointikieleen tai lukemaan lisää asiasta esimerkiksi sivustolta Regular-Expressions.info. Muista kokeilla erilaisia esimerkkejä ja harjoitella säännöllisten lausekkeiden käyttöä, niin saat siitä pian rutinoituneempaa!