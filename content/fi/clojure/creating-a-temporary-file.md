---
title:                "Clojure: Luotaessa Väliaikaisen Tiedoston Luominen"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi luoda väliaikaistiedosto Clojurea ohjelmoitaessa? Väliaikaistiedostot ovat hyödyllisiä silloin kun tarvitsemme tallentaa tilapäisiä tiedostoja, jotka poistetaan käytön jälkeen. Ne voivat myös auttaa välttämään turhia tallennusmuutoksia pysyvissä tiedostoissa. Clojure tarjoaa helpon ja tehokkaan tavan luoda väliaikaistiedostoja.

## Kuinka tehdä

Väliaikaistiedostojen luominen Clojurella on helppoa. Seuraavassa esimerkissä käytämme ```with-open``` -funktiota, joka huolehtii tiedoston avaamisesta ja sulkemisesta automaattisesti.

```Clojure
(with-open [temp-file (java.io.File/createTempFile "temp" ".txt")] 
    (println "Tiedoston nimi:" (.getName temp-file)) 
    (println "Polku:" (.getPath temp-file)) 
    (println "Koko:" (.length temp-file)) 
    (println "Päivämäärä:" (.lastModified temp-file)))
```

Tässä ensin luodaan väliaikaistiedosto käyttäen ```java.io.File/createTempFile``` -funktiota ja tallennetaan se muuttujaan ```temp-file```. Sen jälkeen käytämme erilaisia tiedoston ominaisuuksia ja tulostamme ne konsoliin ```println``` -funktiolla. Lopuksi tiedosto suljetaan automaattisesti ```with-open``` -funktion avulla.

Esimerkkituloste:

```
Tiedoston nimi: temp5359293857508347217.txt
Polku: C:\Users\käyttäjä\AppData\Local\Temp\temp5359293857508347217.txt
Koko: 0
Päivämäärä: 1596368324042
```

## Syventävä sukellus

Väliaikaistiedostojen luominen tapahtuu generoimalla satunnainen nimi ja lisäämällä haluttu tiedostopääte. Nämä tiedostot luodaan oletusarvoisesti järjestelmän väliaikaishakemistoon, mutta voit myös määrittää haluamasi hakemiston parametrina ```java.io.File/createTempFile``` -funktiolle.

Väliaikaistiedostot ovat erityisen hyödyllisiä silloin kun tarvitsemme väliaikaista tallennustilaa esimerkiksi ohjelman suorituksen aikana. Ne voidaan myös helposti poistaa käytön jälkeen, jolloin ne eivät vie turhaa tilaa järjestelmästä.

## Katso myös

- [JavaDoc: File#createTempFile](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#createTempFile(java.lang.String,%20java.lang.String,%20java.io.File))
- [ClojureDocs: with-open](https://clojuredocs.org/clojure.core/with-open) 
- [Etkö ole varma mitä väliaikaistiedoston luomiseen käytetään? Lue tämä artikkeli.](https://www.google.com/)