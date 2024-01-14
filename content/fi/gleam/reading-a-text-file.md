---
title:                "Gleam: Tekstitiedoston lukeminen"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstitiedoston lukeminen on yksi keskeisimmistä ohjelmoinnin taidoista, joka on välttämätön monissa sovelluksissa. Se on erityisen tärkeää, jos haluat käsitellä suuria määriä tiedonkäsittelyä ja tallennusta. Lue tämä artikkeli oppiaksesi, miten voit lukea tekstiä ja hyödyntää sitä Gleam-kielellä.

## Miten

```Gleam 
import io

// Avaaminen ja lukeminen tiedostosta
file := io.open!("tiedosto.txt", "r")
contents := file.read()

// Tulostetaan tiedoston sisältö
case contents {
  Ok(text) -> io.println(text)
  Error(error) -> io.println("Virhe: #(error)")
}
```

Yllä olevassa koodiesimerkissä näet, miten voit avata ja lukea tiedoston Gleam-ohjelmointikielessä. Ensimmäisellä rivillä tuodaan io-moduuli, joka sisältää toimintoja tiedostojen käsittelyyn. Sitten avataan tiedosto käyttäen io.open!-funktiota, joka ottaa ensimmäisenä parametrina tiedoston nimen ja toisena parametrina avaamistilan ("r" tarkoittaa lukemista). Tämän jälkeen voit lukea sisällön kutsuen file.read() -funktiota.

Tulostamme sitten tiedoston sisällön case-lauseen avulla. Jos tiedoston lukeminen onnistuu, sisältö asetetaan muuttujaan "text" ja tulostetaan io.println-funktiolla. Muussa tapauksessa, jos tapahtuu virhe, tulostetaan virheilmoitus.

## Syväluotaus

Tiedostojen lukeminen Gleamilla voi olla hieman monimutkaista, jos haluat tehdä enemmän kuin yksinkertaisesti lukea tiedostoa ja tulostaa sen sisällön. Voit esimerkiksi käsitellä erilaisia formaatteja, kuten CSV- tai JSON-tiedostoja, tai lukea tiedostoa osissa tietyn koon mukaan.

Tähän tarkoitukseen Gleam tarjoaa useita moduuleja, kuten csv, json ja binary, jotka auttavat käsittelemään erilaisia tiedostomuotoja ja datatyyppejä. Voit tutustua näihin moduuleihin Gleam-kirjastosta löytyvien dokumentaatioiden avulla.

## Katso myös

- [Gleam-kirjaston dokumentaatio](https://gleam.run/)
- [Gleam-moduulit tiedostojen käsittelyyn](https://gleam.run/modules/file)
- [Ohjeet teksti-tiedoston lukemiseen Gleamilla](https://gleam.run/articles/reading_text_files)