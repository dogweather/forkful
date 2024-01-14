---
title:    "Ruby: Päivämäärän haku"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Monilla ohjelmointikielillä on sisäänrakennettu toiminto, jolla voi hakea nykyisen päivämäärän ja ajan. Tällainen toiminto on erityisen hyödyllinen esimerkiksi sovelluksissa, jotka vaativat päivämäärän tai ajan tietojen tallentamiseen tai näyttämiseen.

## Kuinka

Tässä esimerkissä käytetään Ruby-kieltä nykyisen päivämäärän ja ajan hakemiseen. Käytämme `Time`-luokan `now`-metodia, joka palauttaa nykyisen ajan. Käytämme myös `strftime`-metodia muotoilemaan haluamamme muodon mukaan.

```Ruby
time = Time.now
puts time #tulostaa nykyisen ajan ja päivämäärän

formatted_time = time.strftime("%d/%m/%Y %H:%M") #muotoilee päivämäärän ja ajan haluamamme muodon mukaan
puts formatted_time #tulostaa esimerkiksi "25/02/2021 14:35"
```

## Syvemmälle

`Time`-luokka sisältää monia erilaisia metodeja ja vaihtoehtoja ajan muotoiluun. `strftime`-metodilla voit muotoilla päivämäärän ja ajan haluamallasi tavalla, esimerkiksi `%d` voi olla päivämäärä numerona ja `%b` kuukauden lyhenteenä. Voit käyttää myös muita luokkia, kuten `Date` ja `DateTime`, saadaksesi lisää vaihtoehtoja ja tarkemman tiedon nykyisestä päivämäärästä ja ajasta.

## Katso myös

- [Ruby Time-luokka](https://ruby-doc.org/core-3.0.0/Time.html)
- [Ruby Date-luokka](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
- [Ruby DateTime-luokka](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/DateTime.html)