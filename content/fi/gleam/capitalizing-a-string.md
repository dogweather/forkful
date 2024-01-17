---
title:                "Pienentäminen merkkijonoksi"
html_title:           "Gleam: Pienentäminen merkkijonoksi"
simple_title:         "Pienentäminen merkkijonoksi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Mitä & miksi?
Stringien isojen kirjainten käyttö tarkoittaa tekstin muuttamista niin, että jokaisen sanan ensimmäinen kirjain muutetaan isoksi. Tätä tehdään yleisesti ohjelmoinnissa, jotta teksti näyttäisi siistimmältä ja helpommin luettavalta.

# Miten:
### Gleam-koodiesimerkki:
```Gleam
"hei, kuka siellä?!" |> String.capitalize
```
### Tulos:
```Gleam
"Hei, kuka siellä?!"
```

### Gleam-koodiesimerkki:
```Gleam
"gleam is awesome" |> String.capitalize
```
### Tulos:
```Gleam
"Gleam is awesome"
```

# Syvempi sukellus:
Isoksi muuttaminen, eli kääntäminen, on ollut käytössä jo kauan ennen ohjelmointia ja sillä oli historiallisesti eri merkitys kuin nykypäivänä. Nykyään isojen kirjainten käyttö liittyy enemmän visuaaliseen ulkoasuun ja helpompaan lukemiseen, mutta historiallisesti sillä pyrittiin myös erottamaan erilaisia sanaluokkia, kuten nimisanoja ja verbejä.

Vaihtoehtojakin löytyy, esimerkiksi pienentäminen eli merkkien muuttaminen pieniksi, mutta se ei ole yhtä yleistä kuin isoksi muuttaminen. Stringien käsittelyyn on myös muita tapoja, kuten pilkkominen tai yhdistäminen, mutta jokaisessa tapauksessa on tärkeää huomioida tietojen muotoilu ja oikeellisuus.

Gleamissa stringien käsittelyyn käytetään apuna moduulia nimeltä String, joka tarjoaa erilaisia toimintoja, kuten capitalize, pienentäminen ja paljon muuta.

# Katso myös:
- [Gleam Language - dokumentaatio](https://gleam.run/documentation/)
- [String Moduuli - Gleam Language dokumentaatio](https://gleam.run/documentation/stdlib/string)