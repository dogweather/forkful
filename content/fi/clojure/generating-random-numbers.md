---
title:                "Satunnaisten lukujen generointi"
html_title:           "Clojure: Satunnaisten lukujen generointi"
simple_title:         "Satunnaisten lukujen generointi"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Satunnaisten numeroiden generoiminen voi olla hyödyllistä monissa tilanteissa, kuten simulaatioissa, peliohjelmoinnissa tai testauksessa. Se voi myös olla hauskaa ja opettavaista kehittäjille, jotka haluavat tutkia satunnaisuuden käsitettä ja sen sovelluksia ohjelmoinnissa.

## Miten

```Clojure
(import java.util.Random)

(def rand (new Random))

(defn generate-random-number [min max]
  (+ min (.nextInt rand (- max min))))

(print (generate-random-number 1 10)) ; Tulostaa satunnaisen numeron väliltä 1-10
```

Meidän tulee ensin tuoda Java-luokka `Random`, joka auttaa meitä generoimaan satunnaisia lukuja. Sitten luomme muuttujan `rand` ja määrittelemme funktion `generate-random-number`, joka ottaa argumenteiksi minimi- ja maksimiluvut, joiden väliltä haluamme generoida satunnaisen luvun. Lopuksi tulostamme generoidun luvun käyttämällä `print`-funktiota.

### Huomioitavaa: 

Useiden satunnaisten lukujen generoimiseen, voi olla hyödyllistä määritellä `rand`-muuttuja funktioksi ja kutsua sitä tarvittaessa.

```Clojure
(defn rand []
  (new Random))

(print (-> (rand) (.nextInt) (+ 1))) ; Tulostaa satunnaisen numeron väliltä 1-10
```

## Syväsukellus

Satunnaislukujen generointi perustuu pseudosatunnaislukugeneraattoreihin, jotka käyttävät algoritmeja tuottamaan lukuja, jotka näyttävät satunnaisilta, mutta ovat itse asiassa ennustettavia. Clojuren `Random`-luokka käyttää `java.lang.Random`-luokan toteutusta, joka käyttää lineaarista kongruenssimenetelmää pseudosatunnaislukujen luomiseen.

On tärkeää huomata, että pseudosatunnaislukujen generointi ei ole todellista satunnaisuutta ja ne voivat alkaa toistaa itseään pitkälti suurempien lukujen generoinnin jälkeen. Tämän vuoksi on suositeltavaa käyttää ylälaita "limit"-arvoa laskiessa satunnaisia lukuja rajattujen joukkojen keskuudessa.

## Katso myös

https://clojure.org/reference/java_interop - Lisätietoa Clojuren Java-vuorovaikutuksesta.

https://docs.oracle.com/javase/8/docs/api/java/util/Random.html - Java-luokan `Random` dokumentaatio.