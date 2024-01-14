---
title:    "Gleam: Nykyisen päivämäärän saaminen"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi: 
Joku voisi haluta saada nykyisen päivämäärän esimerkiksi sovelluksessa tapahtuvien aikaleimojen tallentamiseen tai tarvittavien päivämäärien laskemiseen. 

## Miten: 
Koodiesimerkki, jossa näytetään kuinka saat nykyisen päivämäärän Gleamissa ja miten se tulostetaan komentokehotteelle: 

```Gleam
let tanaan = \Gleam.Date.today()
```

```Gleam
\IO.print("Tänään on ", Gleam.Date.to_string(tanaan), "!")
```

Tämä koodi tuottaa seuraavan tulosteen komentokehotteelle: 

> Tänään on 2020-04-30!

## Syvällinen tutkimus: 
Nykyisen päivämäärän saaminen Gleamissa perustuu "Date" -moduuliin, joka sisältää toimintoja päivämäärän laskemiseen ja muotoiluun. Gleam käyttää ISO 8601 -standardia päivämäärien esittämiseen muodossa "YYYY-MM-DD". Voit tutustua moduulin API-dokumentointiin lisätietojen saamiseksi siitä, kuinka käyttää päivämäärien laskemisessa ja muotoilussa: [Gleam Date - Dokumentointi](https://gleam.run/modules/gleam_date/latest/)

## Katso myös: 
- [Gleam - Verkkosivusto](https://gleam.run/)
- [Gleam - Github](https://github.com/gleam-lang/gleam)
- [Gleam - Opas aloittelijoille](https://gleam.run/book/tour/)

*Kiitos lukemisesta!*