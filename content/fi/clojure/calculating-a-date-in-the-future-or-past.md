---
title:                "Clojure: Ajan laskeminen tulevaisuudessa tai menneisyydessä"
simple_title:         "Ajan laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, jolloin tarvitset tietää tulevia tai menneitä päiviä. Ehkä haluat suunnitella tulevia tapahtumia tai tarkistaa, kuinka kauan sitten jotain tapahtui. Clojuren avulla voit helposti laskea päivien määrän tulevaisuudessa tai menneisyydessä.

## Miten tehdä

```
Clojure (local-date ...)
```

Clojuren `local-date`-funktio mahdollistaa uuden päivän luomisen. Se ottaa parametreinä vuoden, kuukauden ja päivämäärän:

```
Clojure (local-date 2021 7 10)
```

Tämä luo uuden päivän vuonna 2021, heinäkuun 10. päivänä. Voit myös lisätä tai vähentää päiviä luomaasi päivään käyttämällä `plus` tai `minus`-funktioita:

```
Clojure (plus (local-date 2021 7 10) 3)
```

Tämä lisää uuteen päivään kolme päivää, ja näin luodaan päivä heinäkuun 13. päivänä. Voit myös käyttää `minus`-funktiota vähentämään päiviä tai `multiple`-funktiota kertomaan päivien määrän halutulla kertoimella.

## Syvällisempi tarkastelu

Clojuren päivämäärätoiminnot ovat mahdollisia Clojuren sisäänrakennettujen Javan Date ja Time luokkien ansiosta. Tämä tekee päivämäärien käsittelystä erittäin tehokasta ja monipuolista.

Voit myös käyttää muita funktioita, kuten `parse`, `to-date` ja `utc` muuntaaksesi päivämääriä halutun muodon tai aikavyöhykkeen mukaan. Lisätietoja näistä ja muista Clojuren päivämäärätoimintoa löydät virallisesta dokumentaatiosta.

## Katso myös

- Clojuren virallinen dokumentaatio (https://clojure.org/reference/dates)
- Java Date ja Time -luokat (https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- Clojuren päivämäärätoimintoja opastava ohje (https://practicalli.github.io/blog/posts/clojure-dates/)