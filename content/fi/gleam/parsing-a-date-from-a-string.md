---
title:                "Merkkijonosta päivämäärän jäsentäminen"
date:                  2024-01-20T15:36:12.174423-07:00
html_title:           "Bash: Merkkijonosta päivämäärän jäsentäminen"
simple_title:         "Merkkijonosta päivämäärän jäsentäminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Muuttamalla päivämäärän merkkijonosta ymmärrettävään muotoon, ohjelmistot voivat käsitellä aikaa järkevästi. Tätä tarvitaan, koska käyttäjät syöttävät erilaisia ajan esitysmuotoja ja ohjelmat tallentavat aikaa standardiformaatteihin.

## Miten:

```Gleam
import gleam/calendar
import gleam/result

pub fn parse_date(date_string: String) -> result.Result(calendar.DateTime, String) {
  // Kuvitteellinen funktio date_stringin jäsentämiseen
  calendar.parse_rfc3339(date_string)
}

pub fn example() {
  let input = "2023-04-02T14:20:00Z"
  case parse_date(input) {
    Ok(datetime) -> datetime
    Error(error) -> error
  }
}

fn main() {
  example()
}
```

Esimerkkitulostus:
```
Ok(#DateTime<2023-04-02T14:20:00Z>)
```

## Syväsukellus:

Alunperin päivämäärien jäsentäminen syntyi tarpeesta ymmärtää ja vertailla ajanhetkiä tietokoneissa. Historiallisia vaihtoehtoja ovat esimerkiksi simppelit merkkijonolähestymiset. Nykypäivänä, standardit kuten RFC 3339 (ISO 8601:n laajennos) ovat yleistyneet ja jäsentäjiä löytyy jokaisesta modernista ohjelmointikielestä. Gleamissa päivämäärien jäsentäminen vaatii `calendar`-paketin ja usein `result`-moduulin käsittelyyn mahdollisia virheitä varten.

## Katso Myös:

- Gleamin virallinen dokumentaatio: [https://gleam.run](https://gleam.run)
- RFC 3339 standardi: [https://www.ietf.org/rfc/rfc3339.txt](https://www.ietf.org/rfc/rfc3339.txt) 

Muistakaa tarkistaa tuoreimmat versiot paketeista ja dokumentaatiosta.