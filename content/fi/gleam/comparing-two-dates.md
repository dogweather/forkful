---
title:                "Kahden päivämäärän vertailu"
date:                  2024-01-20T17:32:47.426417-07:00
model:                 gpt-4-1106-preview
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
Vertailtaessa kahta päivämäärää, arvioidaan niiden suhdetta toisiinsa. Ohjelmoijat tekevät tätä ajan kulun mittaamiseen, aikataulutuksessa ja aikaleimojen validoinnissa.

## How to:
```gleam
import gleam/calendar.{Date, diff_days}

fn main() {
  let date1 = Date(2023, 4, 15)
  let date2 = Date(2023, 4, 18)

  let days_apart = diff_days(date1, date2)
  assert 3 = days_apart
}

// Output: 3 days
```

## Deep Dive
Vertaamalla päivämääriä ohjelmoijat varmistavat, että tapahtumat tapahtuvat oikeassa järjestyksessä, esimerkiksi sessioiden vanhentumisessa. Aikojen vertailu on perinteitä Unix-aikaleiman käytöstä, mutta nykykielet tarjoavat usein entistä kattavammat työkalut. Gleamissa `calendar`-moduuli tarjoaa funktioita päivämäärien vertailuun ja käsittelyyn. Vaihtoehtoisia menetelmiä, kuten aikaleimojen vertailu, ovat myös käytössä, mutta ne voivat olla alttiimpia aikavyöhykeongelmille ja karkausvuosille.

## See Also
- The Erlang calendar module for comparison: [http://erlang.org/doc/man/calendar.html](http://erlang.org/doc/man/calendar.html)
