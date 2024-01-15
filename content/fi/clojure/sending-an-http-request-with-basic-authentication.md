---
title:                "Http-pyynnön lähettäminen perustason tunnistautumisella"
html_title:           "Clojure: Http-pyynnön lähettäminen perustason tunnistautumisella"
simple_title:         "Http-pyynnön lähettäminen perustason tunnistautumisella"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä miksi haluat lähettää HTTP-pyynnön perusautentikoinnilla. Yksi yleisimmistä on käyttäjätietojen välittäminen palvelimelle, jotta käyttäjä voidaan tunnistaa ja antaa oikeudet tiettyihin resursseihin.

## Kuinka tehdä se

```Clojure
(require '[clj-http.client :as client])

;; Aseta pyynnön osoite
(def url "https://www.example.com/api")

;; Aseta käyttäjän tunnus ja salasana
(def username "kayttajanimi")
(def password "salasana")

;; Luo otsikko, johon käyttäjän tunnus ja salasana koodataan Base64:ksi
(def basic-auth-header (str "Basic " (java.util.Base64/getEncoder 
                           (client/str-join username ":" password))))

;; Lähetä pyyntö
(client/get url {:headers {"Authorization" basic-auth-header}})
```

```
Tulostus:

{:status 200, :headers {"Server" "Apache/2.4.48 (Unix)", "Content-Length" "0", 
"Strict-Transport-Security" "max-age=31536000; includeSubDomains; preload", 
"X-XSS-Protection" "1; mode=block", "X-Content-Type-Options" "nosniff", 
"Date" "Mon, 24 May 2021 12:00:00 GMT"}, :body ""}
```

## Syvemmälle

Perusautentikointi on yksi yksinkertaisimmista tavoista suojata HTTP-pyyntöjä käyttäjätunnuksilla ja salasanalla. Se toimii lähettämällä otsikon, joka sisältää koodatun käyttäjän tunnuksen ja salasanan. Palvelin tarkistaa tämän otsikon ja antaa pääsyn resursseihin vain, jos se vastaa tallennettuja käyttäjätunnuksia ja salasanaa. Base64-koodaus käytetään suojaamaan käyttäjän tunnukset ja salasana välityksessä, mutta se ei tarjoa turvallisuutta salasanan tallennukselle tai siirrolle.

## Katso myös

- [clj-http](https://github.com/dakrone/clj-http) - Clojure-kirjasto HTTP-pyyntöjen tekemiseen.
- [Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme) - Lisätietoa perusautentikoinnista HTTP:ssä.