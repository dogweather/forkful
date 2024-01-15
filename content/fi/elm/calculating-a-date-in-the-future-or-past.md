---
title:                "Ajan laskeminen tulevaisuuteen tai menneisyyteen."
html_title:           "Elm: Ajan laskeminen tulevaisuuteen tai menneisyyteen."
simple_title:         "Ajan laskeminen tulevaisuuteen tai menneisyyteen."
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Monissa sovelluksissa on tarpeen laskea päivä tulevaisuudessa tai menneisyydessä. Ehkä haluat luoda kalenteri-sovelluksen, jossa käyttäjä voi valita tietyn päivän tulevaisuudessa tai tarkastella menneen päivän tapahtumia. Tai ehkä haluat luoda tehtävälistan, joka näyttää käyttäjälle, kuinka monta päivää tiettyyn tehtävään on jäljellä. Onneksi Elm tarjoaa helpon ja tehokkaan tavan laskea päivämääriä tulevaisuudessa tai menneisyydessä.

## Kuinka

Käytämme ```Elm/time```-kirjastoa laskemaan päivämääriä tulevaisuudessa tai menneisyydessä.

### Päivämäärän arvon lisääminen

Voit lisätä päivämäärään tietyn arvon, kuten päiviä, viikkoja, kuukausia tai vuosia, käyttämällä funktiota ```add```:

```Elm
import Time exposing (add, days, weeks, months, years, hours)

add : Time.Posix -> Time.Duration -> Time.Posix
```

Funktio ```add``` ottaa kaksi argumenttia: ensimmäisenä päivämäärän, johon haluat lisätä ja toisena ajanjakson, joka lisätään. Ajanjakson voit määrittää esimerkiksi funktiolla ```days```, joka ottaa argumenttina päivien lukumäärän. Seuraavassa esimerkissä lisätään päivämäärään nykyhetkestä 2 päivää käyttäen funktiota ```add```:

```Elm
Time.now
    |> add (days 2)
-- Output: Time.December 19 2019 12:00:00am
```

Voit myös lisätä päivämäärään useampia ajanjaksoja. Esimerkiksi seuraavassa koodissa lisätään nykyiseen päivään vuosi, kuukausi ja päivä:

```Elm
Time.now
    |> add (months 4)
    |> add (years 1)
    |> add (days 2)
-- Output: Time.June 21 2021 12:00:00am
```

### Päivämäärän vähentäminen

Päivämäärän voi myös vähentää halutulla ajanjaksolla käyttämällä funktiota ```sub```:

```Elm
import Time exposing (sub, days)

sub : Time.Posix -> Time.Duration -> Time.Posix
```

Seuraavassa esimerkissä vähennetään päivämäärästä nykyhetkestä 2 päivää käyttäen funktiota ```sub```:

```Elm
Time.now
    |> sub (days 2)
-- Output: Time.December 15 2019 12:00:00am
```

Voit myös vähentää useampia ajanjaksoja päivämäärästä. Esimerkiksi seuraavassa koodissa vähennetään nykyisestä päivästä 6 kuukautta ja 5 päivää:

```Elm
Time.now
    |> sub (months 6)
    |> sub (days 5)
-- Output: Time.June 10 2019 12:00:00am
```

## Syväluotaus

Internetsivustolla [Elm dokumentaatiossa](https://package.elm-lang.org/packages/elm/time/latest/Time#add) on täydellinen lista funktioista, jota voi käyttää laskeessa päivämääriä tulevaisuudessa tai menneisyydessä.

## Katso myös

- [Elm dokumentaatio - Time-kirjasto](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Elm dokumentaatio - Date-kirjasto](https://package.elm-lang.org/packages/elm-community/date-extra/latest