---
title:                "Clojure: Lähettämällä http-pyyntö perusautentikoinnilla"
simple_title:         "Lähettämällä http-pyyntö perusautentikoinnilla"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

HTTP-pyyntöjen lähettäminen on olennainen osa web-kehitystä. Perusautentikoinnin käyttäminen lisää tietoturvaa ja varmistaa, että vain oikeat käyttäjät pääsevät tiettyihin resursseihin.

## Miten tehdä

```Clojure
;; Lisää tarvittavat kirjastot
(require '[clj-http.client :as http])
(require '[clojure.data.json :as json])

;; Määritä pyynnön osoite ja autentikointitiedot
(def url "https://www.example.com")
(def username "käyttäjänimi")
(def password "salasana")

;; Lähetä GET-pyyntö perusautentikoinnilla
(http/get url {:basic-auth [username password]})

;; Lähetä POST-pyyntö JSON-dataa käyttäen
(http/post url :json {:basic-auth [username password]
                       :body (json/write-str {"kenttä" "arvo"})}))

```

## Tarkempi kuvaus

HTTP-pyynnön lähettäminen perusautentikoinnilla vaatii vain muutaman lisävaiheen verrattuna perinteiseen pyynnön lähettämiseen. Ensimmäiseksi, tarvittavia kirjastoja on tuotava projektiin. Olemme käyttäneet clj-http.client-kirjastoa lähettämään pyyntöjä ja clojure.data.json-kirjastoa parsimaan ja generoimaan JSON-dataa.

Seuraavaksi, määritämme pyyntömme osoitteen ja autentikaatiotiedot. Näihin kuuluu käyttäjänimi ja salasana, jotka annetaan resurssin tarjoajan puolelta.

Lopuksi, lähetämme pyynnön käyttämällä clj-http.client-kirjaston tarjoamia toimintoja. Pyynnön tyyppi määritetään ensimmäisenä parametrina (GET, POST, jne.), osoite annetaan toisena parametrina, ja optional parametrina annetaan määritetyt autentikointitiedot. Lisäksi voimme lähettää myös dataa pyynnön mukana, kuten JSON-muodossa olevia tietoja POST-pyynnöissä.

## Katso myös

- [clj-http library](https://github.com/dakrone/clj-http)
- [Clojure data.json library](https://github.com/clojure/data.json)