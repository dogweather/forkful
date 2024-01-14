---
title:    "Bash: Aloitteiden erottaminen"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Miksi Sukkelan Stringin Outo Maailma

Sano se vaikka ääneen: ohjelmointi voi olla hämmentävää ja ärsyttävää. Se voi tuntua siltä, että sinun täytyy ymmärtää kaikki pienet yksityiskohdat ja hienot ominaisuudet voidaksesi saavuttaa mitä tahansa. Mutta niin ei pitäisi olla!

Yksinkertainen tehtävä, kuten alimerkkijonon erottaminen merkkijonosta, voi olla suuri helpotus, kun opit tekemään sen oikein. Ja tässä blogikirjoituksessa näytän sinulle juuri sen.

## Kuinka Suorittaa Alimerkkijonon Erottaminen

Oletetaan, että sinulla on merkkijono "Hei maailma!" ja haluat vain ottaa siitä "maailma"-osan. Tämä voi tuntua hankalalta, mutta Bash tarjoaa meille muutamia hyödyllisiä työkaluja, joilla tämä tehtävä voidaan tehdä helposti.

Ensinnäkin, meillä on komento `cut`, joka leikkaa merkkijonon tietyistä osista. Voimme käyttää sitä seuraavasti:

```
Bash $ merkkijono = "Hei maailma!"
Bash $ echo $ {string | leikkaa -d, -f2}
```

Tämä komento leikkaa merkkijonon pilkkujen välillä ja ottaa toisen sarakkeen, joka tässä tapauksessa on "maailma!".

Voit myös käyttää komentoa `awk` samaan tarkoitukseen:

```
Bash $ merkkijono = "Hei maailma!"
Bash $ echo $ {string | awk -F "," '{print $ 2}'}
```

Tässä "awk" käyttää "," desimaalilukuna ja ottaa toisen osan, joka on "maailma!".

Ja vielä yksi tapa on käyttää Bashin sisäänrakennettua `substring` -toimintoa:

```
Bash $ merkkijono = "Hei maailma!"
Bash $ echo $ {string: 4}
```

Tämä ottaa merkkijonosta 4. merkistä eteenpäin, jättäen "maailma!" osan jäljelle.

Kaikki nämä vaihtoehdot saavuttavat saman tuloksen, joten voit valita haluamasi tavan. Katso vain, mikä sopii parhaiten tiettyyn tilanteeseen.

## Syventävää Tietoa Alimerkkijonon Erottamisesta

Nyt kun olet oppinut helposti erottelemaan alimerkkijonoja Bashilla, on tärkeää ymmärtää, että yksittäisten merkkijonojen lisäksi voit myös käyttää näitä komentoja suurille merkkijonojoukoille. Esimerkiksi voit lukea merkkijonoja tiedostosta ja erotella ne siellä.

Voit myös käyttää vaihtoehtoja, kuten `-s`, joka ohittaa tyhjät rivit tai `-n`, joka poistaa toisen sarakkeen ja jättää vain ensimmäisen.

## Katso myös

- [Bash Scripting Tutorial](https://blogit.foi.fi/skongkpeli/bash-skriptauksen-perusteet/)
- [GNU Bash Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [Videotutoriaali "Bashin perusteet"](https://www.youtube.com/watch?v=TRvgJc_eY1E)