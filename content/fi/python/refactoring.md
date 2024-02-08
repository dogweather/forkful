---
title:                "Koodin refaktorointi"
aliases:
- fi/python/refactoring.md
date:                  2024-01-26T03:37:24.863446-07:00
model:                 gpt-4-0125-preview
simple_title:         "Koodin refaktorointi"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/refactoring.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Refaktorointi on prosessi, jossa olemassa olevaa tietokonekoodia rakennetaan uudelleen—muutetaan faktorointia—muuttamatta sen ulkoista käyttäytymistä. Ohjelmoijat tekevät sitä siistiäkseen koodia, parantaakseen luettavuutta ja tehdäkseen siitä helpommin ylläpidettävän ja laajennettavan, kaikki lisäämättä uusia ominaisuuksia.

## Miten:
Kuvitellaan, että sinulla on koodinpätkä, joka laskee ja tulostaa suorakulmion pinta-alan ja piirin annettujen pituus- ja leveysmittojen perusteella. Se toimii, mutta se on toistuvaa ja hieman sotkuista.

```python
# Alkuperäinen versio
pituus = 4
leveys = 3

# Laske pinta-ala ja piiri
pinta_ala = pituus * leveys
piiri = 2 * (pituus + leveys)

print("Pinta-ala:", pinta_ala)
print("Piiri:", piiri)
```

Voimme refaktoroida tämän kapseloimalla toiminnallisuuden funktioihin, mikä tekee koodista järjestäytyneemmän ja uudelleenkäytettävän:

```python
# Refaktoroitu versio

def laske_pinta_ala(pituus, leveys):
    return pituus * leveys

def laske_piiri(pituus, leveys):
    return 2 * (pituus + leveys)

# käyttö
pituus = 4
leveys = 3

print("Pinta-ala:", laske_pinta_ala(pituus, leveys))
print("Piiri:", laske_piiri(pituus, leveys))
```

Molemmat pätkät tulostavat saman tuloksen:
```
Pinta-ala: 12
Piiri: 14
```

Mutta refaktoroitu versio on siistimpi ja erottaa huolenaiheet toisistaan, mikä tekee yhden laskelman päivittämisestä helpompaa vaikuttamatta toiseen.

## Syväsukellus
Refaktoroinnin juuret ovat ohjelmistosuunnittelun alkuaikoina, kun ohjelmoijat ymmärsivät, että koodia voitiin—ja pitäisi—parantaa, vaikka se jo "toimisi". Martin Fowlerin merkittävä kirja "Refactoring: Improving the Design of Existing Code" artikuloi monet ydintekniikat ja -periaatteet. Hän sanoi kuuluisasti, "Kuka tahansa hölmö voi kirjoittaa koodia, jonka tietokone ymmärtää. Hyvät ohjelmoijat kirjoittavat koodia, jonka ihmiset voivat ymmärtää."

Vaihtoehtoja refaktoroinnille voisi sisältää koodin uudelleenkirjoittamisen alusta tai pienet säädöt ilman systemaattista parantamista. Kuitenkin refaktorointi on yleensä kustannustehokkaampi kuin uudelleenkirjoitus ja vähemmän riskialtis kuin ad-hoc-muutokset. Toteutuksen yksityiskohdat voivat olla erityisiä kullekin ohjelmointiparadigmalle; kuitenkin objektiivinen ohjelmointi soveltuu erityisen hyvin refaktorointiin, erityisesti tekniikoilla kuten metodien eriyttäminen (kuten `laske_pinta_ala` ja `laske_piiri` funktiot), sisäänrakentaminen, ominaisuuksien siirtäminen objektien välillä ja metodien tai muuttujien uudelleennimeäminen selkeyden vuoksi.

Pythonissa refaktorointiin käytetään usein työkaluja kuten `PyCharm`, jossa on sisäänrakennettuja refaktorointiominaisuuksia, tai `rope`, Python-kirjasto erityisesti refaktorointia varten. Varovaisuus versionhallinnan, kuten `git`, käytössä refaktoroinnin aikana on erittäin suositeltavaa muutosten inkrementelliseen seurantaan.

## Katso myös
Lisätietoa kaipaaville, sukella seuraaviin resursseihin:
- Martin Fowlerin kirja: [Refactoring: Improving the Design of Existing Code](http://www.refactoring.com/)
- Python-refaktorointi `rope`n avulla: [GitHub - rope](https://github.com/python-rope/rope)
- PyCharmin refaktorointidokumentaatio: [Jetbrains PyCharm Refactoring Source Code](https://www.jetbrains.com/help/pycharm/refactoring-source-code.html)
- Refactoring.guru: [Refaktorointi ja suunnittelumallit](https://refactoring.guru/refactoring)
- Clean Code -luennot Uncle Bob (Robert C. Martin) mukaan: [Clean Code - Uncle Bob / Luento 1](https://www.youtube.com/watch?v=7EmboKQH8lM)
