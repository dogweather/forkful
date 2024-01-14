---
title:                "Go: Tulostuksen virhelähdön tulostaminen"
programming_language: "Go"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi voisi hyödyntää debug-tulostuksen käyttöä Go-ohjelmoinnissa. Se voi auttaa ratkaisemaan virheitä ja vianmäärityksessä sekä parantaa ohjelman tehokkuutta. 

## Ohjeet

Debug-tulostuksen käyttö on helppoa Go-koodissa. Se voidaan tehdä kahdella tavalla: 

1. Käyttämällä "fmt" -pakettia: ```Go 
fmt.Println("Debug-tulostus") 
```

2. Käyttämällä log-kirjastoa: ```Go 
log.Println("Debug-tulostus") 
```

Näiden kahden tavan välillä ei ole suurta eroa, joten voit valita oman mieltymyksesi mukaan. 

Debug-tulostuksen avulla voit tulostaa muuttujien, sisäisten funktioiden ja muiden tietojen arvoja, mikä auttaa sinua ymmärtämään, mitä koodisi tekee ja mikä aiheuttaa mahdollisia virheitä. 

## Syväsukellus 

Debug-tulostuksen käyttöön on monia syvällisiä tapoja, joista voit oppia lisää. Voit esimerkiksi käyttää log-kirjastoa erilaisten tasojen (esim. info, virhe, varoitus) asettamiseksi tai lisätä aikaleimat tulosteisiisi. Voit myös tutkia "reflect" -pakettia, joka mahdollistaa laajemman ja tarkemman tietojen tarkastelun debug-tulostuksen kautta. 

## Katso myös 

- (Go-pakettien käyttöönotto)[https://golang.org/doc/code.html]
- (Go-opas ja virheenkorjaus)[https://learnxinyminutes.com/docs/fi-fi/go-fi/]