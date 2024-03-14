---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:12.197401-07:00
description: "Lokitus ohjelmistokehityksess\xE4 on prosessi, jossa kirjataan tietoa\
  \ ohjelman suorituksesta, jonka tarkoituksena on seurata sen k\xE4ytt\xE4ytymist\xE4\
  \ ja\u2026"
lastmod: '2024-03-13T22:44:56.057643-06:00'
model: gpt-4-0125-preview
summary: "Lokitus ohjelmistokehityksess\xE4 on prosessi, jossa kirjataan tietoa ohjelman\
  \ suorituksesta, jonka tarkoituksena on seurata sen k\xE4ytt\xE4ytymist\xE4 ja\u2026"
title: Lokitiedostot
---

{{< edit_this_page >}}

## Mikä & Miksi?

Lokitus ohjelmistokehityksessä on prosessi, jossa kirjataan tietoa ohjelman suorituksesta, jonka tarkoituksena on seurata sen käyttäytymistä ja diagnosoida ongelmia. Ohjelmoijat toteuttavat lokituksen seuratakseen ohjelmiston suorituskykyä, debugata virheitä ja varmistaa järjestelmän turvallisuuden ja noudattamisen, mikä tekee siitä korvaamattoman työkalun sovelluksen ylläpidossa ja analysoinnissa.

## Miten:

Gossa lokitusta voidaan toteuttaa käyttäen standardikirjaston pakettia `log`. Tämä paketti tarjoaa yksinkertaisia lokitusominaisuuksia, kuten kirjoittamisen standarditulosteeseen tai tiedostoihin. Aloitetaan perusesimerkillä lokituksesta standarditulosteeseen:

```go
package main

import (
	"log"
)

func main() {
	log.Println("Tämä on peruslokiviesti.")
}
```

Tuloste:
```
2009/11/10 23:00:00 Tämä on peruslokiviesti.
```

Aikaleima lokiviestin alussa lisätään automaattisesti `log`-paketin toimesta. Seuraavaksi tutkitaan, miten lokitetaan tiedostoon standarditulosteen sijaan:

```go
package main

import (
	"log"
	"os"
)

func main() {
	file, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	log.SetOutput(file)
	log.Println("Tämä lokiviesti menee tiedostoon.")
}
```

Nyt toteutetaan edistyneempi käyttötapaus: lokitusmuodon mukauttaminen. Go sallii sinun luoda mukautetun lokittajan käyttämällä `log.New()`:

```go
package main

import (
	"log"
	"os"
)

func main() {
	logger := log.New(os.Stdout, "OMA LOKI: ", log.Ldate|log.Ltime|log.Lshortfile)
	logger.Println("Tämä on mukautettu lokiviesti.")
}
```

Tuloste:
```
OMA LOKI: 2009/11/10 23:00:00 main.go:11: Tämä on mukautettu lokiviesti.
```

Tässä esimerkissä jokainen lokiviesti alkaa etuliitteellä "OMA LOKI: " ja sisältää päivämäärän, ajan ja lähdekooditiedoston sijainnin.

## Syväsukellus

Go-standardikirjaston `log`-paketti on suoraviivainen ja riittävä moniin sovelluksiin, mutta siitä puuttuu joitakin kehittyneempiä ominaisuuksia, joita löytyy kolmannen osapuolen lokituskirjastoista, kuten rakennoitu lokitus, lokien kierto ja tasopohjainen lokitus. Paketit, kuten `zap` ja `logrus`, tarjoavat nämä edistyneet ominaisuudet ja ovat Go-yhteisössä arvostettuja niiden suorituskyvyn ja joustavuuden vuoksi.

Esimerkiksi rakennoitu lokitus mahdollistaa tietojen lokittamisen rakenteisessa muodossa (kuten JSON), mikä on erityisen hyödyllistä nykyaikaisissa pilvipohjaisissa sovelluksissa, joissa lokia saatetaan analysoida erilaisilla työkaluilla tai palveluilla. `zap` on erityisesti tunnettu sen korkeasta suorituskyvystä ja pienestä allokointitarpeesta, mikä tekee siitä sopivan sovelluksiin, joissa nopeus ja tehokkuus ovat kriittisiä.

Historiallisesti Gossa lokitus on kehittynyt merkittävästi kielen syntyhetkestä lähtien. Golangin varhaisissa versioissa tarjottiin peruslokitusominaisuuksia, jotka näemme `log`-paketissa. Kuitenkin, kun kieli kasvoi suosiossa ja Golla kirjoitettujen sovellusten monimutkaisuus lisääntyi, yhteisö alkoi kehittää kehittyneempiä lokituskirjastoja tarpeidensa täyttämiseksi. Nykyään, vaikka standardi `log`-paketti pysyykin vaihtoehtona yksinkertaisille sovelluksille, monet kehittäjät kääntyvät näiden kolmannen osapuolen ratkaisujen puoleen monimutkaisempien lokitusvaatimusten kanssa.
