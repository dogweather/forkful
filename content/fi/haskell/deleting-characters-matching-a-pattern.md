---
title:    "Haskell: Kuvioiden mukaisien merkkien poistaminen."
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Haskellissa on useita tapoja käsitellä merkkijonoja ja niiden osia. Usein kohtaamme tilanteita, joissa haluamme poistaa tietystä merkkijonosta tietynlaisia merkkejä. Tässä blogikirjoituksessa tarkastelemme, miten voi poistaa merkkejä, jotka vastaavat tiettyä kaavaa.

## Miten

Haskellissa meillä on käytössämme `Data.Text` -kirjasto, joka tarjoaa useita hyödyllisiä toimintoja merkkijonojen käsittelyyn. Yksi niistä on `Data.Text.replace`, jota voimme käyttää poistamaan merkkejä tietystä merkkijonosta.

```Haskell
import Data.Text (replace)

main = do
  let str = "Hello, World!"
  let newStr = replace "l" "" str
  print newStr
```

Tämä koodinpätkä tulostaa "Heo, Word!", sillä olemme poistaneet kaikki "l"-merkit merkkijonosta. Voimme myös käyttää tähän `Data.Text.filter` -funktiota, joka ottaa argumentiksi predikaatin ja poistaa merkit, jotka täyttävät sen ehdot.

```Haskell
import Data.Text (filter)

main = do
  let str = "Haskell on loistava kieli!"
  let newStr = Data.Text.filter (/= 'a') str
  print newStr
```

Tulostus tässä tapauksessa olisi "Hskell on loistv kieli!", sillä olemme poistaneet kaikki "a"-merkit merkistä.

## Syvempi sukellus

Haskellin merkkijonojen käsittelyssä on tärkeää muistaa, että merkkijonot ovat muuttumattomia, eli niitä ei voi muokata suoraan. Sen sijaan käytämme erilaisia toimintoja ja palautamme uuden merkkijonon muutoksien jälkeen. Lisäksi merkkijonoja käsitellessä tulee kiinnittää huomiota siihen, että `Data.Text` -kirjasto käyttää unicode-merkkejä, joten merkkien määrittelyssä tulee käyttää `Char`-tyypin sijaan `Data.Text` -kirjaston `Text`-tyyppiä.

Merkkien poistaminen on myös yleinen tehtävä rekursiivisissa funktioissa, joissa käydään läpi pitkä merkkijono ja poistetaan siitä tietyt merkit.

## Katso myös

- [Haskellin `Data.Text` -kirjaston dokumentaatio](https://hackage.haskell.org/package/text/docs/Data-Text.html)
- [Haskellin merkkijonojen käsittelyn opas](https://stackoverflow.com/questions/57820747/haskell-how-to-process-text)