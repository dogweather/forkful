---
title:                "Merkkijonosta päivämäärän jäsentäminen"
date:                  2024-01-20T15:34:28.271230-07:00
html_title:           "Bash: Merkkijonosta päivämäärän jäsentäminen"
simple_title:         "Merkkijonosta päivämäärän jäsentäminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Datapäivämäärän parsinta merkkijonosta muuttaa tekstimuotoisen päivämäärän ohjelmallisesti käsiteltävään muotoon. Käsittely mahdollistaa päivämäärän vertailut, ajanlaskut ja muotoilut.

## How to:
Päivämäärien parsinta Bashissa käyttäen `date` komentoa:

```Bash
# Muunnetaan merkkijono päivämääräksi
parsed_date=$(date -d '2023-04-01 14:00' '+%Y-%m-%d %H:%M:%S')
echo $parsed_date
# Tulostuu: 2023-04-01 14:00:00

# Muunnetaan toiseen aikavyöhykkeeseen
helsinki_date=$(TZ='Europe/Helsinki' date -d '2023-04-01 14:00 UTC' '+%Y-%m-%d %H:%M:%S')
echo $helsinki_date
# Tulostuu: 2023-04-01 17:00:00
```

## Deep Dive
Bashin `date` komento on ollut osana UNIX-järjestelmiä alusta asti, ja se on vakiintunut työkalu ajan käsittelyyn skripteissä. Yleisiä vaihtoehtoja datan parsintaan ovat: `awk`, `sed` ja modernit ohjelmointikielet kuten Python tai Ruby, joissa päivämääräkäsittely on usein monipuolisempaa.

`date` komennon käyttö voi vaihdella käyttöjärjestelmän ja Bash-version mukaan, mutta yllä annetut esimerkit toimivat useimmissa ympäristöissä.

## See Also
- GNU Coreutils `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Bash-skriptausopas: https://www.tldp.org/LDP/Bash-Beginners-Guide/html/
- Ajan käsittely POSIX-järjestelmissä: https://pubs.opengroup.org/onlinepubs/009695399/utilities/date.html
