---
title:                "Haskell: Säännöllisten lausekkeiden käyttö"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Miksi käyttää säännöllisiä lausekkeita?

Säännölliset lausekkeet ovat erittäin hyödyllisiä työkaluja, kun käsitellään merkkijonoja. Ne mahdollistavat monimutkaisten etsintä- ja korvaustoimintojen suorittamisen helposti ja tehokkaasti. Käyttämällä säännöllisiä lausekkeita voidaan esimerkiksi tarkistaa, vastaavatko syötteet tietylle muotolle tai suorittaa laaja kirjo hakutoimintoja, kuten merkkijonojen haku ja muuttaminen.

# Miten käyttää säännöllisiä lausekkeita Haskellissa?

Säännölliset lausekkeet ovat kiinteä osa Haskellin standardikirjastoa ja niiden käyttö on helppoa. Ensimmäiseksi tulee tuoda ```Text.Regex.Posix``` moduuli, jossa säännöllisten lausekkeiden toiminnot sijaitsevat. Tämän jälkeen voidaan käyttää joko ```=~``` tai ```=~~``` operaattoria säännöllisen lausekkeen ja syötteen vertailuun.

Esimerkiksi, jos haluamme tarkistaa vastaako syöte muotoa "ABC123", käytämme seuraavaa koodia:

```Haskell
import Text.Regex.Posix 

checkPattern :: String -> Bool
checkPattern input = input =~ "ABC123"
```

Tämän jälkeen voimme suorittaa funktiota käyttämällä esimerkiksi seuraavaa syötettä:

```Haskell
checkPattern "ABC123" --> True
```

# Syvemmät tiedot säännöllisten lausekkeiden käytöstä

Säännöllisten lausekkeiden mallikieli on tiivis ja tehokas, mutta samalla myös monimutkainen. Sen opetteleminen ja vakioiden ja ominaisuuksien ymmärtäminen voi vaatia aikaa ja harjoittelua. Yksi huomionarvoinen seikka on se, että Haskellin säännölliset lausekkeet noudattavat POSIX-standardia, mikä voi poiketa joistakin muista ohjelmointikielistä.

Voit opetella säännöllisten lausekkeiden syvempiä ominaisuuksia ja käyttötapoja tutustumalla POSIX-standardiin tai lukemalla lisätietoja Haskellin dokumentaatiosta.

# Katso myös

- [Haskellin tekstikäsikirja, säännölliset lausekkeet](https://hackage.haskell.org/package/regex-posix-0.95.2/docs/Text-Regex-Posix.html)
- [POSIX-säännölliset lausekkeet](https://www.regular-expressions.info/posix.html)