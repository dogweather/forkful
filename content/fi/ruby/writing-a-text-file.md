---
title:    "Ruby: Toisen tiedoston luominen"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Miksi

Miksi kirjoittaisimme tekstitiedostoa ohjelmoidessa? Tekstitiedostot ovat hyödyllisiä tapoja tallentaa ja jakaa tietoa ohjelmien välillä. Niitä voi myös käyttää esimerkiksi tiedostojen varmuuskopioimiseen tai käyttäjän asetusten tallentamiseen.

## Kuinka

Tekstitiedoston kirjoittaminen Rubyllä on helppoa. Käytämme ensin ```File.open``` -metodia, joka avaa tiedoston ja tarjoaa meille ```File``` -olion tiedoston käsittelyyn. Voimme sitten käyttää ```puts``` tai ```write``` -metodeita kirjoittaaksemme haluamamme tekstin tiedostoon.

```
# Avataan tiedosto nimeltään "testi.txt" ja tallennetaan se muuttujaan "tiedosto"
tiedosto = File.open("testi.txt", "w")

# Kirjoitetaan teksti tiedostoon käyttämällä "puts" -metodia
puts tiedosto, "Tervetuloa lukijat!"

# Suljetaan tiedosto
tiedosto.close
```

## Syväsukellus

Edellä mainittu esimerkki avaa ja kirjoittaa tekstitiedoston, mutta mitä jos haluamme lisätä tekstiä jo olemassa olevaan tiedostoon? Tämä onnistuu käyttämällä ```File.open``` -metodin toista parametria, joka määrittelee tiedoston avausmuodon. Esimerkiksi, jos haluamme lisätä tekstiä tiedoston loppuun, käytämme "a" sijasta "w".

```
# Avataan tiedosto nimeltään "testi.txt" lisäystilassa ja tallennetaan se muuttujaan "tiedosto"
tiedosto = File.open("testi.txt", "a")

# Lisätään teksti loppuun käyttämällä "puts" -metodia
puts tiedosto, "Kiitos lukemisesta!"

# Suljetaan tiedosto
tiedosto.close
```

Voimme myös käyttää ```File``` -luokan muita metodeita, kuten ```read``` ja ```foreach```, tekstitiedoston sisällön lukemiseen tai käsittelyyn.

## Katso myös

- [Ruby dokumentaatio: File class](https://ruby-doc.org/core-2.3.0/File.html)
- [Ilmainen Ruby-kurssi (englanniksi)](https://www.codecademy.com/learn/learn-ruby)