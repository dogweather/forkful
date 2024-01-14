---
title:    "Gleam: Kahden päivämäärän vertailu"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Miksi vertailla kahta päivämäärää?

Vertailemalla kahta päivämäärää voit selvittää, kumpi päivämäärä on suurempi tai pienempi ja kuinka monta päivää niiden välillä on.

## Kuinka tehdä?

Vertailemalla kahta päivämäärää voit käyttää "compare_dates" -toimintoa ja antaa sille kaksi päivämäärää. Tämän jälkeen voit verrata saatuja tuloksia ja saada selville onko ensimmäinen päivämäärä pienempi, suurempi vai yhtä suuri kuin toinen päivämäärä.

```Gleam
compare_dates(Date.from_yyyy_mm_dd(2021, 10, 5), Date.from_yyyy_mm_dd(2021, 10, 15))
```

```
Date.same // false
Date.before // true
Date.after // false
```

## Syvemmälle aiheeseen

Vertailemalla päivämääriä Gleamissa voit myös selvittää kuinka monta päivää niiden välissä on. Voit käyttää tähän "days_between_dates" -toimintoa ja antaa sille kaksi päivämäärää.

```Gleam
days_between_dates(Date.from_yyyy_mm_dd(2021, 10, 5), Date.from_yyyy_mm_dd(2021, 10, 15))
```

```
10 // päivien määrä
```

Voit myös vertailla päivämääriä sekä kellonaikoja käyttäen "compare_dates_times" -toimintoa ja antaa sille kaksi päivämäärää sekä niiden kellonajat.

```Gleam
compare_dates_times(DateTime.from_yyyy_mm_dd_hh_mm_ss_ms(2021, 10, 5, 10, 30, 0, 0), DateTime.from_yyyy_mm_dd_hh_mm_ss_ms(2021, 10, 5, 12, 30, 0, 0))
```

```
DateTime.same // false
DateTime.before // true
DateTime.after // false
```

# Katso myös

- [Gleamin virallinen dokumentaatio](https://gleam.run/documentation)
- [Kuinka käsitellä päivämääriä Gleamissa](https://blog.gleam.run/comparing-and-manipulating-dates-in-gleam/)
- [Kuinka käyttää Gleamia suuremmissa projekteissa](https://gleam.run/configuration-and-projects)