---
title:                "Clojure: Ajan laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Monissa tilanteissa saattaa olla tarpeellista laskea tietty päivämäärä tulevaisuudessa tai menneisyydessä eteenpäin. Tässä blogikirjoituksessa käydään läpi miten tätä tehdään Clojure-kielellä.

## Kuinka tehdä

Yksi tapa laskea tulevaisuuden tai menneisyyden päivämääriä Clojurella on käyttää `clj-time` -kirjastoa, joka tarjoaa hyödyllisiä funktioita päivämäärien käsittelyyn.

### Tulevaisuuden päivämäärä

Voimme käyttää `clj-time.core/plus` -funktiota lisäämään päivämäärään haluamamme määrän päiviä. Esimerkiksi, jos haluamme laskea päivän kolme päivää eteenpäin tänään:

```Clojure
(let [tänään (t/today)
      tulevaisuudessa (t/plus tänään (t/days 3))]
  tulevaisuudessa)
;; => "2021-08-03T00:00:00.000000000"
```

### Menneisyyden päivämäärä

Menneisyyden päivämäärän laskeminen on samanlainen kuin tulevaisuuden päivämäärän laskeminen, mutta käytämme `clj-time.core/minus` -funktiota. Esimerkiksi, jos haluamme laskea päivän neljä päivää taaksepäin tänään:

```Clojure
(let [tänään (t/today)
      menneisyydessä (t/minus tänään (t/days 4))]
  menneisyydessä)
;; => "2021-07-28T00:00:00.000000000"
```

## Syvemmälle aiheeseen

Voimme myös käyttää `clj-time.core/date-time` -funktiota asettaaksemme tietyn päivämäärän kokonaisella vuosiluvulla, kuukaudella ja päivällä. Esimerkiksi, jos haluamme laskea päivän tulevaisuudessa vuonna 2022:

```Clojure
(let [tulevaisuudessa (t/date-time 2022 1 1)]
  tulevaisuudessa)
;; => "2022-01-01T00:00:00.000000000"
```

On myös mahdollista muuttaa päivämäärää eri muodoiksi käyttämällä `clj-time.format/unparse` -funktiota. Esimerkiksi, jos haluamme tulostaa päivämäärän tekstimuodossa:

```Clojure
(let [tänään (t/date-time 2021 8 1)
      tekstimuodossa (t/format tänään "dd/MM/yyyy")]
  tekstimuodossa)
;; => "01/08/2021"
```

## Katso myös

- `clj-time` -kirjasto: https://github.com/clj-time/clj-time
- `clj-time` -dokumentaatio: https://clj-time.github.io/clj-time/
- `clj-time` -esimerkit: https://github.com/clj-time/clj-time/blob/master/test/de/siegmar/clj-time/test/core.clj