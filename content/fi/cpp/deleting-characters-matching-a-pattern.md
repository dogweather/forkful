---
title:                "Puolan mukaiset merkkien poistaminen"
html_title:           "C++: Puolan mukaiset merkkien poistaminen"
simple_title:         "Puolan mukaiset merkkien poistaminen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Saatat joskus joutua poistamaan merkkejä tietyllä kaavalla ohjelmaasi. Ehkä haluat poistaa kaikki välilyönnit tai erikoismerkit käyttäjän syötteestä. Tässä artikkelissa näytämme, miten voit tehdä tämän C++:lla.

## Miten

```C++
// Alustetaan merkkijono, josta haluamme poistaa merkkejä
std::string teksti = "Tämä on esimerkki!";
// Alustetaan uusi merkkijono, johon tallennamme muokatun version
std::string muokattu_teksti = "";

// Käydään läpi jokainen merkki alkuperäisessä merkkijonossa
for (int i = 0; i < teksti.length(); i++) {
  // Tallennetaan jokainen merkki muuttujaan "kirjain"
  char kirjain = teksti[i];
  // Tarkistetaan, onko "kirjain" välilyönti
  if (kirjain != ' ') {
    // Jos ei ole, lisätään se uuteen merkkijonoon
    muokattu_teksti += kirjain;
  }
}

std::cout << muokattu_teksti << std::endl;
```

**Tulostus:**

`Tämäonesimerkki!`

Tässä esimerkissä käytämme for-silmukkaa ja if-lausetta tarkistaaksemme kutakin merkkiä ja lisäämme ne uuteen merkkijonoon vain, jos ne eivät ole välilyöntejä. Voit muuttaa if-ehtoa vastaavasti, jos haluat poistaa muita merkkejä.

## Syventävä tietoa

Mikäli haluat poistaa merkkejä tietyn kaavan perusteella, voit käyttää myös säännöllisiä lausekkeita (regex). C++:ssa voit käyttää std::regex -kirjastoa tähän tarkoitukseen. Toinen tapa on käyttää kirjaston Boost.Regex tarjoamia työkaluja, jotka ovat usein tehokkaampia ja monipuolisempia kuin std::regex. Muista kuitenkin, että säännöllisten lausekkeiden käyttö vaatii hieman enemmän ohjelmointitaitoja.

## Katso myös

- [std::regex - C++ Reference](https://en.cppreference.com/w/cpp/regex)
- [Boost.Regex - Official Documentation](https://www.boost.org/doc/libs/1_75_0/libs/regex/doc/html/index.html)