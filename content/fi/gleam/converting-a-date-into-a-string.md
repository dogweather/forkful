---
title:    "Gleam: Päivämäärän muuntaminen merkkijonoksi"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Miksi

Jokaisessa ohjelmointikielessä on tarpeen muuttaa päivämäärä tekstimuotoon tai päinvastoin. Gleamin avulla tämä tehtävä on helppo ja sujuva.

# Kuinka tehdä se

Gleamin avulla päivämäärän muuttaminen tekstimuotoon on yksinkertaista ja suoraviivaista. Käytä vain `Date.format` -funktiota ja anna haluttu muoto parametrina.

```Gleam
let date = Date.from_ymd(2021, 11, 15)
let formatted_date = Date.format(date, "dd.MM.yyyy")
// Output: 15.11.2021
```

Voit myös muuttaa päivämäärän tekstimuotoon tietystä aikavyöhykkeestä riippumatta käyttämällä `Date.format_zone` -funktiota.

```Gleam
let date = Date.from_ymd(2021, 11, 15)
let formatted_date = Date.format_zone(date, "dd.MM.yyyy", "Europe/Helsinki")
// Output: 15.11.2021
```

# Syvällinen sukellus

Gleamin `Date` -moduuli sisältää useita muita hyödyllisiä funktioita päivämäärän käsittelyyn. Voit esimerkiksi muuttaa päivämäärän Unix-timestamppina käyttämällä `Date.to_unix` -funktiota ja päinvastoin `Date.from_unix` -funktiota. Voit myös lisätä tai vähentää päiviä tai aikoja yksinkertaisesti käyttämällä `Date.add_days`, `Date.add_months` tai `Date.add_years` -funktioita.

# Katso myös

- [Gleam - Date](https://gleam.run/modules/datetime)
- [Gleamin päivämäärän muodonmuutosohjeet](https://gleam.run/getting-started/datetime)