---
title:                "Työskentely jsonin kanssa"
html_title:           "Clojure: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/working-with-json.md"
---

{{< edit_this_page >}}

Mitä & Miksi?

JSON eli JavaScript Object Notation (JavaScriptin objektimuoto) on tiedostomuoto, jota käytetään tietojen tallentamiseen ja siirtämiseen. Se on yleisesti käytetty formaatti erityisesti web-sovelluksissa ja rajapinnoissa. JSON on suosittu ohjelmoijien keskuudessa, koska se on helppo lukea ja kirjoittaa, sekä sopii hyvin käytettäväksi erilaisten ohjelmointikielien välillä.

Kuinka:

Clojure tarjoaa valmiin kirjaston käsittelemään JSON-muotoisia tietoja. Tämän kirjaston nimi on clj-json ja se tarjoaa helpon tavan lukea ja muokata JSON-dataa Clojure-koodissa.

```Clojure
; Lisää ensin clj-json-kirjasto projektiisi
[clj-json "1.7.4"]

; Tämän jälkeen voit käyttää kirjaston funktioita JSON-muotoisen datan käsittelyyn
(use 'clj-json.core)

; Lukee JSON-tiedoston ja palauttaa Clojure-maps-rakenteen
(json/read-str "{}")
; => {}

; Luo uuden JSON-tiedoston Clojure-maps-rakenteesta ja tallentaa sen tiedostoon
(spit "testi.json" (json/generate-string {:nimi "Olli" :ika 30}))
; => nil

; Lukee JSON-tiedoston ja palauttaa Clojure-maps-rakenteen
(json/parse-string (slurp "testi.json"))
; => {:nimi "Olli", :ika 30}
```

Syvemmät tiedot:

JSON-muoto kehitettiin vuonna 2001 ja se perustuu JavaScriptin syntaksiin. Sen tarkoituksena oli korvata XML-kieli, jossa syntaksin monimutkaisuus oli ongelmana. JSON:lla ei ole kaikkia XML:n ominaisuuksia, mutta se on silti hyvin käytännöllinen ja sopii hyvin web-sovellusten käyttöön.

JSON:n vaihtoehtona käytetään usein XML:ää ja YAML:ia. Ne kaikki ovat tiedostomuotoja tietojen tallentamiseen ja siirtämiseen, mutta ne eroavat hieman toisistaan syntaksinsa ja ominaisuuksiensa osalta.

JSON-muoto koostuu arvoista ja avaimista muodossa "avain:arvo", joita kutsutaan myös sanakirjoiksi tai animoiksi. Tämä vastaa Clojuren maps-rakennetta ja siksi JSON-datan käsittely on helppoa Clojurella.

Katso myös:

- Clj-json kirjaston dokumentaatio: https://github.com/lynaghk/clj-json
- JSON spesifikaatio: https://www.json.org/json-fi.html