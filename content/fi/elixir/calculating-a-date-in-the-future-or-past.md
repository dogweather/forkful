---
title:                "Elixir: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmointissa on tarpeellista laskea tietty päivämäärä tulevaisuudesta tai menneisyydestä. Tämä voi johtua esimerkiksi lomien suunnittelusta, tapahtumien aikatauluttamisesta tai muista syistä. Elixir-ohjelmoinnissa tämä on suhteellisen helppo toteuttaa käyttäen sisäänrakennettuja funktioita. Seuraavassa jaettelen lyhyesti, miten tämän voitaisiin tehdä.

## Ohjeet

Laskemiseen päivämäärä tulevaisuudessa tai menneisyydessä käytetään Elixirin `Calendar`-moduulissa olevia funktioita. Ensiksi tarvitsemme päivämäärän, josta haluamme liikkua eteen- tai taaksepäin. Tämän jälkeen voimme käyttää `Date.add`- tai `Date.subtract`-funktioita lisäämään tai vähentämään päiviä, viikkoja, kuukausia tai vuosia.

```Elixir
# Lisätään yksi viikko päivämäärään 1.1.2022
date = Date.from_iso8601("20220101")
new_date = Date.add(date, 7, :week)
IO.inspect(new_date)

# Tulostaa: ~D[2022-01-08]
```

Voimme myös käyttää `Date.shift`-funktiota liikkumaan eteen- tai taaksepäin tietyn ajanjakson kuten minuuttien, tuntien tai päivien suhteen. `Date.next_day`- ja `Date.prev_day`-funktioiden avulla voimme siirtyä tiettyyn viikonpäivään, kuten seuraavaan maanantaihin tai edelliseen sunnuntaihin.

```Elixir
# Siirrytään eteenpäin 2 päivää päivämäärästä 1.1.2022
date = Date.from_iso8601("20220101")
new_date = Date.shift(date, 2, :day)
IO.inspect(new_date)

# Tulostaa: ~D[2022-01-03]

# Siirrytään ensi maanantaihin
date = Date.from_iso8601("20220101")
new_date = Date.next_day(date, :monday)
IO.inspect(new_date)

# Tulostaa: ~D[2022-01-03]
```

## Syvempi sukellus

Elixirin `Calendar`-moduuli käyttää Gregoriaanista kalenteria laskiessaan päivämäärää tulevaisuudessa tai menneisyydessä. Tämä tarkoittaa, että se ottaa huomioon karkausvuodet ja muut ajanlaskun säännöt.

On myös tärkeää huomata, että Elixirin aikavyöhykkeet tallentuvat kokonaisina minuutteina UTC-aikajanaan verrattuna. Tämä voi vaikuttaa laskemiseen, mikäli käytössäsi on tietty aikavyöhyke.

## Katso myös

- [Elixirin Kalenteri-dokumentaatio](https://hexdocs.pm/elixir/Calendar.html)
- [Date-tyyppi Elixirin dokumentaatiossa](https://hexdocs.pm/elixir/Date.html)
- [Uusien ominaisuuksien julkaisupäivämäärä laskurin avulla Elixir](https://elixir-lang.org/blog/2015/12/31/elixir-v1-2-0-released/)