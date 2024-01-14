---
title:    "Gleam: Tekstin etsiminen ja korvaaminen"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Miksi etsiä ja korvata tekstiä?

Etsiä ja korvata tekstiä on erittäin hyödyllinen työkalu ohjelmointimaailmassa, joka säästää aikaa ja vaivaa. Sen avulla voit nopeasti löytää ja korvata haluamasi tekstin tiedostoista tai koodista, mikä tekee koodin muokkaamisesta ja päivittämisestä helpompaa.

# Miten?

Etsiä ja korvata tekstiä Gleam-ohjelmointikielellä on helppoa. Voit käyttää sisäänrakennettua `String.replace()`-funktiota tai asentaa lisäosia, kuten `gleamix/search_replace`-lisäosan. Käytämme tässä esimerkkiä `gleamix/search_replace`-lisäosasta.

Ensinnäkin, asennamme lisäosan Gleam-projektiimme käyttämällä `mix`-komennon `add`-parametria:

```
mix add gleamix/search_replace
```

Sitten tuomme `SearchReplace`-moduulin haluamallemme tiedostolle `using`-komennolla:

```
using SearchReplace
```

Seuraavaksi voimme käyttää `SearchReplace.replace()`-funktiota etsiäksemme ja korvataksemme haluamamme tekstin. Esimerkiksi, jos haluamme korvata kaikki merkkijonot "hello" merkkijonolla "hei", meidän tarvitsee vain kutsua `SearchReplace.replace()`-funktiota ja antaa molemmat merkkijonot parametreinä:

```
let uusi_merkkijono = SearchReplace.replace("hello", "hei", vanha_merkkijono)
```

Tämän jälkeen `uusi_merkkijono` sisältää korvatun merkkijonon `vanhan_merkkijonon` sijaan.

# Syventävä sukellus

Gleam-ohjelmointikielen `String.replace()`-funktio ja `gleamix/search_replace`-lisäosa hyödyntävät erittäin suorituskykyistä algoritmia, joka käsittelee jopa suuria tiedostoja tehokkaasti. Voit myös käyttää säännöllisiä lausekkeita etsiäksesi monimutkaisempia tekstimuutoksia. Lisäksi, voit käyttää `SearchReplace.replace()`-funktiota myös muissa tiedostotyypeissä, kuten JSON-tiedostoissa.

# Katso myös

- `gleamix/search_replace` -lisäosa: https://github.com/gleam-extras/search_replace
- Gleam-dokumentaatio: https://gleam.run/documentation/