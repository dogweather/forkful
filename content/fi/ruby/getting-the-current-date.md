---
title:                "Päivämäärän saaminen"
html_title:           "Ruby: Päivämäärän saaminen"
simple_title:         "Päivämäärän saaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmoinnin projekteissa on tarvetta käyttää ajantasaista päivämäärää, esimerkiksi tapahtumien ajoittamisessa tai tietokantojen päivityksissä. Onneksi Rubyn nykyinen versio tarjoaa yksinkertaisen tavan saada tiedot tämän hetken päivämäärästä.

## Miten

Rubyn `Time`-luokan `now`-metodi palauttaa nykyisen ajan ja päivämäärän yhtenä `Time`-oliona. Voit käyttää sitä näyttämään tämän hetken kellonajan yksinkertaisella komennolla:

```Ruby
puts Time.now
```

Tämä tulostaa nykyisen ajan ja päivämäärän muodossa `vuosi-kuukausi-päivä tunti:minuutti:sekunti +0000`. Voit myös käyttää `strftime`-metodia määrittääksesi halutun tulostusmuodon. Se ottaa argumenttinaan merkkijonon, jossa on määritelty haluttu muoto. Esimerkiksi:

```Ruby
puts Time.now.strftime("%d.%m.%Y")
```

Tämä tulostaa nykyisen päivämäärän muodossa `päivä.kuukausi.vuosi`.

## Syvemmälle

Vaikka `Time.now` on kätevä tapa saada tieto tämän hetken päivämäärästä, on myös muita tapoja saada sama tieto. Voit esimerkiksi käyttää `Date`-luokan `today`-metodia, joka palauttaa nykyisen päivämäärän yksinkertaisena `Date`-oliona. Tai voit käyttää `DateTime`-luokan `now`-metodia saadaksesi tiedon päivämäärän ja kellonajan yhdistelmästä.

On myös huomattava, että `Time`-oliot ovat tavallisesti luotettavia vain vuodesta 1970 lähtien. Ennen sitä ajan laskenta voi olla epätarkkaa tai jopa virheellistä.

## Katso myös

- [Ruby Time Documentation](https://ruby-doc.org/core-3.0.1/Time.html)
- [Ruby Date Documentation](https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/Date.html)
- [Ruby DateTime Documentation](https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/DateTime.html)