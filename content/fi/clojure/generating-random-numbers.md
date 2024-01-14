---
title:                "Clojure: Satunnaisten lukujen generointi"
programming_language: "Clojure"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Miksi käyttää satunnaislukugeneraattoria?

Satunnaislukugeneraattorit ovat tärkeä osa ohjelmoinnin maailmaa, ja niitä käytetään monissa erilaisissa sovelluksissa ja ohjelmointikielissä. Clojure on yksi näistä kielistä ja silläkin on omat satunnaislukugeneraattorinsa. Miksi sitten haluaisit käyttää satunnaislukugeneraattoria Clojure-ohjelmointisi kanssa? Yksinkertaisesti sanottuna, satunnaislukugeneraattorit mahdollistavat satunnaisuuden ja arvaamattomuuden ohjelmassasi. Tämä voi olla hyödyllistä esimerkiksi simulaatioissa tai pelien kehittämisessä.

# Kuinka käyttää satunnaislukugeneraattoria Clojuressa?

Jos haluat käyttää Clojuren sisäänrakennettua satunnaislukugeneraattoria, voit käyttää funktiota ```(rand)```. Tämä funktio tuottaa pseudosatunnaiseen lukuun desimaaliluvun väliltä 0 ja 1. Jos haluat määrittää haluamasi arvojen välisen alueen, voit käyttää funktiota ```(rand-nth [aloitus lopetus])```, jossa aloitus ja lopetus ovat haluamasi lukujen rajat. Voit myös käyttää Clojuren ```random```-kirjastoa, joka tarjoaa enemmän vaihtoehtoja satunnaislukujen generointiin. Esimerkiksi voit käyttää funktiota ```(rand-int 100)``` tuottaaksesi satunnaisen kokonaisluvun väliltä 0 ja 99. Alla on esimerkkikoodia satunnaislukujen generoinnista ja sen tuottamasta tulosteesta:

```Clojure
;; tuottaa pseudosatunnaisen luvun väliltä 0 ja 1
(rand)

;; tuottaa kokonaisluvun väliltä 0 ja 99
(rand-int 100)

;; tuottaa satunnaisen luvun väliltä 1 ja 10
(rand-nth [1 10])
```

Tuloste:

```
0.743517076636204

64

7
```

# Syventävä tieto satunnaislukujen generoinnista

Satunnaislukujen generointi ei ole täysin satunnaista, vaan kyseessä on oikeasti pseudosatunnaislukugenerointi. Tämä tarkoittaa sitä, että generoitu luku seuraa tiettyä kaavaa ja on toistettavissa. Clojuren ```rand```-funktio käyttää **Mersenne Twister** -algoritmia generoidakseen satunnaisia lukuja. Tämä algoritmi on suosittu ja tehokas pseudo-satunnaislukugeneraattori monissa ohjelmointikielissä, ja se on myös testattu laajasti.

# Katso myös
- [Clojuren dokumentaatio - Random](https://clojuredocs.org/clojure.core/rand)
- [Clojuren dokumentaatio - Random library](https://clojuredocs.org/clojure.core/rand-int)
- [Mersenne Twister - Wikipedia (englanniksi)](https://en.wikipedia.org/wiki/Mersenne_Twister)