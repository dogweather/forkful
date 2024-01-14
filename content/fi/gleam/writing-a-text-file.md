---
title:                "Gleam: Tiedoston kirjoittaminen"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

Miksi kirjoittaisit Gleam-tekstitiedoston?

Jos olet ohjelmoija, joka haluaa laajentaa taitojaan ja oppia uutta kieltä, Gleam voi olla loistava valinta. Yksi Gleamin ominaisuuksista on sen kyky käsitellä tekstitiedostoja, mikä tekee siitä täydellisen työkalun tekstin käsittelyyn ohjelmissasi.

Kuinka kirjoittaa Gleam-tekstitiedosto?

Kirjoittaminen Gleam-tekstitiedosto on helppoa! Aloita luomalla uusi teksti-tiedosto ja varmista, että olet tallentanut sen mitä tahansa nimeä ja lisännyt ".gleam" tiedostopäätteen. Sitten voit aloittaa kirjoittamisen!

```Gleam
# This is a Gleam text file
# Let's write some text!
Teksti = "Tervetuloa Gleam-tekstitiedoston maailmaan!"
näytä(Teksti)
```

Tulostus:
```
Tervetuloa Gleam-tekstitiedoston maailmaan!
```

Syöte ja tuloste voidaan myös tallentaa muuttujiin ja käyttää niitä myöhemmin ohjelmassa.

```Gleam
# This is a Gleam text file
# Let's write some text!
Teksti = "Tervetuloa Gleam-tekstitiedoston maailmaan!"
KaksinkertainenTeksti = Teksti + Teksti
näytä(KaksinkertainenTeksti)
```

Tulostus:
```
Tervetuloa Gleam-tekstitiedoston maailmaan!
Tervetuloa Gleam-tekstitiedoston maailmaan!
```

Voit myös lisätä muuttujiin käyttäen Gleamin mallilauseita. Esimerkiksi voit lisätä uuden rivin tekstin loppuun käyttämällä "muokkaa()" funktiota:

```Gleam
# This is a Gleam text file
# Let's write some text!
Teksti = "Tervetuloa Gleam-tekstitiedoston maailmaan!"
muokkaa(Teksti, "Tämä on uusi rivi")
näytä(Teksti)
```

Tulostus:
```
Tervetuloa Gleam-tekstitiedoston maailmaan!
Tämä on uusi rivi
```

Syöte kannattaa myös aina varmistaa käyttämällä "tarkista()" funktiota, joka tarkistaa, jos syöte vastaa tiettyä kriteeriä.

Syvällisempi tieto Gleam-tekstitiedoston kirjoittamisesta

Gleamilla on myös muita hyödyllisiä funktioita, kuten "luoTiedosto()" ja "kirjoitaTiedostoon()", jotka antavat sinulle mahdollisuuden luoda ja kirjoittaa tiedostoja ohjelmissasi. Voit myös käyttää "avaaTiedosto()" ja "lueRivi()" funktioita lukeaksesi olemassa olevan tekstitiedoston sisältöä.

Joten, jos haluat oppia lisää tekstitiedostojen käsittelystä Gleamilla, suosittelemme tutustumaan Gleamin virallisiin dokumentaatioihin ja oppaisiin.

Katso myös

Tutustu näihin hyödyllisiin resursseihin saadaksesi lisätietoja Gleamin käytöstä:

- Gleamin virallinen dokumentaatio (englanniksi): https://gleam.run/

- Gleamin GitHub-sivusto (englanniksi): https://github.com/gleam-lang/gleam

- Gleam-tutorial (englanniksi): https://gleam.run/tour/