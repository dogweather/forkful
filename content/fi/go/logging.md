---
title:                "Lokitus"
date:                  2024-01-26T01:04:10.629516-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lokitus"

category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/logging.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Lokitus tarkoittaa tapahtumien, tilojen ja tietovirtojen kirjaamista sovelluksessa. Ohjelmoijat tekevät sitä virheiden diagnosoimiseksi, suorituskyvyn seuraamiseksi ja sovelluksen operatiivisen terveyden tarkkailemiseksi—tehden siitä ohjelmistojen mustan laatikon vastineen lentokoneissa.

## Kuinka:
Golangoilla lokitusta voidaan hoitaa monin eri tavoin, standardikirjaston `log`-paketista kolmansien osapuolien kirjastoihin kuten `logrus` ja `zap`. Tässä on yksinkertainen esimerkki sisäänrakennetun `log`-paketin käytöstä:

```Go
package main

import (
	"log"
	"os"
)

func main() {
	// Luo lokitiedosto
	logFile, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
	if err != nil {
		log.Fatal(err)
	}
	defer logFile.Close()

	// Aseta loki kirjoittamaan tiedostoon
	log.SetOutput(logFile)

	// Kirjaa joitakin tapahtumia
	log.Println("Sovelluksen käynnistys...")
	// ... sovelluksen logiikka tässä ...
	log.Println("Sovellus päättyi onnistuneesti.")
}
```

Jos suoritat tämän koodin, et näe tulostetta terminaalissa, koska kaikki menee `app.log`-tiedostoon. Tässä on kurkistus siitä, mitä lokitiedostosta löytyisi:

```
2023/01/02 15:04:05 Sovelluksen käynnistys...
2023/01/02 15:05:01 Sovellus päättyi onnistuneesti.
```

## Syväsukellus
Lokitus ohjelmoinnissa juontaa juurensa varhaisimpiin tietokoneisiin, jossa insinöörit kirjaimellisesti löysivät vikoja (koita, tarkalleen ottaen) murskautuneina laitteistossa, ja he lokittivat ne! Nykyaikana lokitus on kehittynyt monimutkaisten järjestelmien toiminnan ymmärtämisen edistyneeksi tavaksi.

Vaikka Golangin `log`-paketti on varsin yksinkertainen, se voi riittää perussovelluksiin. Kuitenkin, kun puhutaan moderneista hajautetuista järjestelmistä, tai kun tarvitaan hienostuneempaa kontrollia lokitulosteen yli (kuten eri vakavuustasot), saatat haluta tutustua kestävämpiin ratkaisuihin.

Kolmansien osapuolien arvostamat lokikirjastot, kuten `logrus` ja `zap`, tarjoavat rakenteellista lokitusta, mikä tarkoittaa, että voit kirjata monimutkaisia datatyyppejä kuten JSON, tehdessäsi lokitietoja helpommin tulkitseviksi, erityisesti yhdessä lokinhallintajärjestelmien kuten ELK Stackin tai Splunkin kanssa.

Kun lokitusstrategian toteutusta harkitaan, on myös olennaista ajatella suorituskykyseuraamuksia. Korkean suorituskyvyn lokituskirjastot on optimoitu vähentämään vaikutusta sovelluksen läpikulkuun ja viiveeseen. Esimerkiksi `zap` kehuu nopeasta, vähäisestä allokaatiomuotoilustaan, joka voi olla ratkaisevan tärkeää reaaliaikajärjestelmissä.

Lokikirjastojen lisäksi myös lokiformaateista ja standardeista on hyödyllistä mainita. Rakenteelliset lokiformaatit, kuten JSON, voivat olla valtavan tehokkaita käytettäessä yhdessä lokiprosessointijärjestelmien kanssa. Toisaalta pelkät tekstilokit ovat ihmisen luettavissa, mutta haastavampia jäsentää ohjelmallisesti.

## Katso Myös
Jos haluat sukeltaa syvemmälle Golangin lokituskyvykkyyksiin, nämä resurssit saattavat olla hyödyllisiä:

- Golangin blogi lokituksesta: https://blog.golang.org/logging
- `logrus`, rakenteellinen lokittaja Golangille: https://github.com/sirupsen/logrus
- `zap`, nopea, rakenteellinen, tasoitettu lokittaja: https://github.com/uber-go/zap
- ELK Stack (Elasticsearch, Logstash, Kibana) lokianalytiikkaan: https://www.elastic.co/what-is/elk-stack
- Vertailu Golangin lokituskirjastoista: https://www.loggly.com/blog/benchmarking-5-popular-golang-logging-libraries/
