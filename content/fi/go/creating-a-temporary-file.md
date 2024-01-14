---
title:                "Go: Väliaikaisen tiedoston luominen"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Monesti ohjelmoijat joutuvat tekemään väliaikaisia tiedostoja, jotka eivät ole tarpeen ohjelman suorituksen jälkeen. Tämä voi olla esimerkiksi väliaikainen tallennusohjelma, joka käsittelee tietoja ennen niiden lähettämistä. Tässä blogikirjoituksessa kerromme, miksi ja miten voit luoda väliaikaisia tiedostoja Go-ohjelmoinnilla.

## Kuinka luoda väliaikainen tiedosto Go-ohjelmoinnilla

Go-ohjelmoinnissa väliaikaisen tiedoston luominen on suhteellisen helppoa. Voit käyttää standardikirjaston io/ioutil-pakettia luodaksesi ja käsitelläksesi tiedostoja.

```Go
import "io/ioutil"

// Luodaan väliaikainen tiedosto käyttäen ioutil.TempFile-funktiota
file, err := ioutil.TempFile("", "esimerkki")
if err != nil {
  fmt.Println(err)
}
defer os.Remove(file.Name())

// Kirjoitetaan tiedostoon haluttu tieto
fmt.Println("Kirjoitetaan dataa tiedostoon", file.Name())
testdata := []byte("Hello, maailma!")
if _, err := file.Write(testdata); err != nil {
  fmt.Println(err)
}
```

Tämän koodin suorittamisen jälkeen olet luonut väliaikaisen tiedoston, jonka nimi alkaa "esimerkki" ja pidät muuttujassa "file". Voit käyttää tätä tiedostoa esimerkiksi tallentamaan väliaikaisen datan ja sen jälkeen lähettää tiedoston tai poistaa sen ohjelman suorituksen lopuksi. Alla oleva koodipätkä osoittaa, kuinka voit lukea tiedoston sisällön ja tulostaa sen konsolille.

```Go
// Luetaan ja tulostetaan tiedoston sisältö
data, err := ioutil.ReadFile(file.Name())
if err != nil {
  fmt.Println(err)
}
fmt.Println(string(data))

// Output: Hello, maailma!
```

## Syvemmälle väliaikaisen tiedoston luomiseen

Go:n ioutil.TempFile-funktio luo tiedoston hakemistoon, joka on määritetty standardiympäristömuuttujalla "TMPDIR". Jos tätä muuttujaa ei ole määritetty, se käyttää oletushakemistoa "/tmp". Jos haluat määrittää halutun hakemiston, voit antaa sen parametrina funktiolle. Esimerkiksi:

```Go
os.Setenv("TMPDIR", "/koti/kayttajanimi/tmp/") // Määritetään maksimitilaa
file, err := ioutil.TempFile("", "esimerkki")
```

Voit myös käyttää ioutil.TempDir-funktiota luodaksesi väliaikaisen hakemiston sijaan tiedostoa, jos se on sinulle hyödyllisempää.

## Katso myös

- [Go:n offikaalit dokumentaatiot ioutil.TempFile](https://golang.org/pkg/io/ioutil/#TempFile)
- [Iotil.TempFile-koodiesimerkit](https://gobyexample.com/temporary-files)