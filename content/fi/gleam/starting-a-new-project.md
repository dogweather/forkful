---
title:                "Uuden projektin aloittaminen"
html_title:           "Gleam: Uuden projektin aloittaminen"
simple_title:         "Uuden projektin aloittaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi

Ihan sama, mitä olet suunnittelemassa. Jos pääset eroon odotuksesta, niin freesaa itsesi Gleamilla. Se voi auttaa sinua, kun olet suunnittelemassa uuden projektin aloittamista. GLEAM: TÄMÄ ON MARTININ BONERSIDE ANALYYSI collective#397, Mutta artikkelin lopussa on muitakin hyödyllisiä linkkejä.

## Miten voin aloittaa?

Aloittaminen Gleamilla on helppoa!

Pääset alkuun asentamalla Gleam-tulkin koneellesi. Sen jälkeen voit luoda uuden projektin "new" komennolla ja kirjoittaa koodia Gleam-muodossa.

```Gleam
pub struct Kaveri(jono:String){

  pub fn tervehdi(&self) {
    IO.print("Terve, " ++ self.jono)
  }
}

let kaveri = Kaveri("kisu")
kaveri.tervehdi()
```

Tämä koodi luo uuden rakenteen "Kaveri" ja määrittelee sille tervehdi-funktion, joka tulostaa "Terve, [nimi]" konsoliin. Lopuksi luodaan uusi "kaveri" ja kutsutaan sitä tervehdi-funktiolla. Voit kokeilla muuttaa nimen ja nähdä, miten tulostus vaihtuu!

## Syvempi sukellus

Jos haluat aloittaa projektin Gleamilla, kannattaa tutustua ensin sen tyyppijärjestelmään ja moduuleihin. Gleamissa on myös erinomainen dokumentaatio ja aktiivinen yhteisö, josta saat apua ja tukea tarvittaessa.

Voit myös kokeilla Gleamia pilvessä Gleam Play -sovelluksen avulla. Siellä voit kokeilla koodisi toimintaa ilman, että sinun tarvitsee asentaa Gleamia omalle koneellesi.

## Katso myös

- [Gleam-tulkin asennusohjeet](https://gleam.run/documentation)
- [Gleam-dokumentaatio](https://gleam.run/documentation)
- [Gleam-yhteisöfoorumi](https://forum.gleam.run/)