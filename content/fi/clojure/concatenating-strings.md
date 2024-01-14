---
title:                "Clojure: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointikielissä stringsien yhdistäminen on yksi perustavanlaatuisista tehtävistä. Clojure ei ole poikkeus. Concatenation suorittaa stringien yhdistämisen yhdeksi stringiksi. Tässä blogipostissa kerron syvemmin, miksi joinkin tapauksissa voi olla hyödyllistä yhdistää stringsia ja miten se tehdään Clojurella.

## Miten

```Clojure
;Yhden stringin yhdistäminen:
(println (str "Tämä on " "yksi string " "yhdistettynä."))
; Output:
; Tämä on yksi string yhdistettynä.

;Kahden tai useamman stringin yhdistäminen:
(println (apply str ["Ensin " "yhdistyy " "nämä " "ja sitten " "nämä."]))
; Output:
; Ensin yhdistyy nämä ja sitten nämä.
```

## Syväluotaus

Stringsien yhdistämiselle voi olla monia syitä. Yksi yleisin on erilaisten käyttöliittymien tekeminen. Esimerkiksi, jos haluat näyttää käyttäjälle tietyn otsikon ja sisällön, voit yhdistää nämä kaksi stringiä yhdeksi ja tulostaa sen sitten käyttäjälle. Toinen hyödyllinen käyttötapaus voi olla datan muotoilu. Jos esimerkiksi haluat muotoilla tietokannasta haetun datan taulukoksi, voit yhdistää tarvittavat stringit ja tulostaa ne sitten järjestelyä varten.

On myös tärkeää muistaa, että stringsien yhdistäminen ei ole vain useiden stringien "liittämistä" toisiinsa. Concatenation tuottaa uuden stringin, joka on yhdistettyjen stringien yhdistelmä. Siksi, jos haluat vain yhdistää stringejä niiden välillä ilman välilyöntejä, voit käyttää esimerkiksi funktiota `str/join`.

## Katso myös

- [Clojure.org - Strings](https://clojure.org/reference/strings)
- [ClojureDocs - str](https://clojuredocs.org/clojure.core/str)
- [ClojureDocs - join](https://clojuredocs.org/clojure.string/join)