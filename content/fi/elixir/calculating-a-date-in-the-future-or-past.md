---
title:                "Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
html_title:           "Elixir: Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
simple_title:         "Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä tarkoittaa päivämäärän määrittämistä tietyn ajanjakson päässä nykyajasta. Tätä tehdään usein ohjelmoinnissa, jotta voidaan suunnitella ja seurata tapahtumia tulevaisuudessa tai hakea tietoja menneisyydestä.

## Miten:

### Tulevaisuuden päivämäärän laskeminen:

```Elixir
DateTime.utc_now() |> DateTime.add(weeks: 2)
```

Tämä koodi esittää päivämäärän kaksi viikkoa nykyhetkestä. Tämän koodin tulostus voi näyttää esimerkiksi tältä:

```Elixir
#=> ~N[2021-12-28 00:00:00]
```

### Menneisyyden päivämäärän laskeminen:

```Elixir
~N[2021-12-28 00:00:00] |> DateTime.add(days: -30)
```

Tämä koodi esittää päivämäärän 30 päivää taaksepäin. Koodin tuloste voi olla esimerkiksi:

```Elixir
#=> ~N[2021-11-28 00:00:00]
```

## Syventyminen:

Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä on ollut tärkeä osa ohjelmointia jo pitkään. Ennen vanhaan tämä tehtiin manuaalisesti laskemalla päiviä huolellisesti tai käyttämällä erikoisia kalentereita. Nykyään tämän voi tehdä helposti käyttämällä ohjelmointikieliä, kuten Elixir.

Toinen tapa laskea päivämäärä tulevaisuudessa tai menneisyydessä on käyttää Date-pakettia Elixirissä. Tämä tarjoaa lisää toiminnallisuuksia, kuten arkipäivien huomioimisen päivämäärän laskemisessa.

Implementaatiotiedot:

Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä perustuu aika- ja päivämäärätoimintojen hyödyntämiseen Elixirin sisäänrakennetussa DateTime-moduulissa. Tästä moduulista löytyy monia hyödyllisiä toimintoja, kuten päivän lisääminen tai vähentäminen nykyhetkestä tai tietyn päivämäärän luominen.

## Katso myös:

- [Elixir Date-paketti](https://hexdocs.pm/elixir/Date.html)
- [DateTime-moduulin dokumentaatio](https://hexdocs.pm/elixir/DateTime.html)
- [Aikapäiväkirja - laskeskeleva päivämääräpainike](https://www.matematiikkalehti.com/wp-content/uploads/matikka5-2013-sivut_26_31.pdf) historiallisesta kontekstista