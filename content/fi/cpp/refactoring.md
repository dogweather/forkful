---
title:                "Koodin refaktorointi"
date:                  2024-01-26T01:17:20.710323-07:00
model:                 gpt-4-0125-preview
simple_title:         "Koodin refaktorointi"
programming_language: "C++"
category:             "C++"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/refactoring.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Koodin uudelleenjärjestäminen (refaktorointi) on prosessi, jossa muutetaan tietokoneohjelman sisäistä rakennetta muuttamatta sen ulkoista toimintaa. Ohjelmoijat tekevät sitä siistiäkseen koodiaan, jolloin se on helpompi ymmärtää, ylläpitää ja laajentaa.

## Kuinka:

Kuvittele, että sinulla on funktio, joka tekee hieman liikaa, kuten tämä kömpelö metodi, joka alustaa olion ja suorittaa myös lokitusta:

```C++
#include <iostream>

class Widget {
public:
    void init(bool verbose) {
        // Alustuslogiikka
        // ...

        // Laajasanainen lokitus
        if (verbose) {
            std::cout << "Widget alustettu!" << std::endl;
        }
    }
};

// Käyttö:
Widget w;
w.init(true);
```

Tuloste:
```
Widget alustettu!
```

Tämän uudelleenjärjestämisen puhtaammiksi, fokusoituneemmiksi metodeiksi voisi näyttää tältä:

```C++
#include <iostream>

class Widget {
public:
    void init() {
        // Vain alustuslogiikka
        // ...
    }

    void logInit() const {
        std::cout << "Widget alustettu!" << std::endl;
    }
};

// Käyttö:
Widget w;
w.init();
w.logInit();
```

Tämä muutos ei ole muuttanut ohjelman toimintaa, mutta tekee `Widget`-luokasta modulaarisemman ja sen käytön selkeämmäksi.

## Syväsukellus

Nykyajan tuntemamme refaktoroinnin konseptilla on juurensa 1980-luvun Smalltalk-ohjelmointiyhteisöissä ja se tuli vahvasti tunnetuksi Martin Fowlerin kirjasta "Refactoring: Improving the Design of Existing Code" vuodelta 1999. Nykyään refaktorointi on modernin ohjelmistokehityksen ydinosa, joka on integroitu erilaisiin kehitysmenetelmiin, kuten Agileen ja TDD:hen (Test-Driven Development).

Puhuttaessa vaihtoehdoista refaktoroinnille, siirrytään uudelleenkirjoittamisen tai uudelleensuunnittelun alueelle. Refaktorointi on strategista ja asteittaista, kun taas uudelleenkirjoitus saattaa hylätä olemassa olevan koodin uuden ratkaisun hyväksi. Uudelleensuunnittelu puolestaan voi sisältää merkittävämpiä muutoksia, mukaan lukien toiminnallisuuden muuttaminen, mikä ei ole puhdasoppisen refaktoroinnin tavoite.

Refaktoroinnin toteutuksen yksityiskohdat voivat olla varsin yksityiskohtaisia. Monet 'koodin hajut' voivat vaatia refaktorointia, kuten pitkät metodit, suuret luokat tai koodin duplikaatit. Olemassa on automatisoituja työkaluja, jotka voivat auttaa refaktoroinnissa, kuten "Clang-Tidy" C++:lle, joka voi huomata ongelmia ja jopa soveltaa joitakin korjauksia.

Lisäksi, refaktorointi vaatii vankan testisarjan varmistaakseen, että toiminnallisuus pysyy muuttumattomana. Ilman testejä olet käytännössä lentämässä sokeana ja riskeeraat regressioita.

## Katso Myös

Syvemmän ymmärryksen saavuttamiseksi refaktoroinnista ja lisäesimerkkien näkemiseksi, saatat haluta tutustua:

- Martin Fowlerin klassikkotekstiin "Refactoring: Improving the Design of Existing Code" perusideoista ja strategioista.
- `Clang-Tidy`-dokumentaatioon osoitteessa https://clang.llvm.org/extra/clang-tidy/ automatisoidusta refaktoroinnin tuesta C++:lle.
- Michael Feathersin kirjaan "Working Effectively with Legacy Code", joka tarjoaa tekniikoita turvalliseen refaktorointiin vähemmän täydellisissä olemassa olevissa koodikannoissa.
