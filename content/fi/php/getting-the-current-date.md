---
title:                "Nykyisen päivämäärän hankkiminen"
date:                  2024-01-20T15:15:54.631758-07:00
html_title:           "Bash: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"

category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
PHP:llä päivämäärän noutaminen tarkoittaa nykyhetken ajan kaappaamista. Koodaajat tekevät tämän, kun heidän sovelluksensa tarvitsevat aikaleimoja, ajastustoimintoja tai päivämäärästä riippuvaa logiikkaa.

## How to: (Kuinka tehdä:)
```PHP
<?php
// Noudetaan nykyhetki
$nyt = new DateTime();
echo $nyt->format('Y-m-d H:i:s'); // esim. tulostus: 2023-03-15 14:45:02
```

Saat myös aikavyöhykkeen mukaan:
```PHP
$nytHelsinki = new DateTime('now', new DateTimeZone('Europe/Helsinki'));
echo $nytHelsinki->format('Y-m-d H:i:s'); // esim. tulostus: 2023-03-15 16:45:02
```

## Deep Dive (Sukellus syvemmälle)
PHP:n `DateTime` luokkaa on käytetty jo vuodesta 2005, kun se esiteltiin PHP 5.2:ssa. Se tarjoaa joustavan tavan käsitellä päivämääriä. Aikaisemmin, `date()` -funktiota käytettiin usein, mutta `DateTime` tarjoaa enemmän toiminnallisuutta, kuten aikavyöhykkeiden käsittelyä.

Vaihtoehtoisia tapoja saada nykyinen päivämäärä:
- `time()` palauttaa sekuntien määrän Unix-ajanlaskun alusta
- `date('Y-m-d')` on yksinkertaisempi tapa, mutta ei tarjoa objekti-orientoitua joustavuutta

`DateTime` toimii sisäisesti DateTimeImmutable-luokkaan nähden, mikä tarkoittaa, että se ei muuta alkuperäistä DateTime-objektia, kun siihen tehdään muutoksia. Tämä tekee koodista ennustettavampaa ja virheettömämpää.

## See Also (Katso myös)
- [PHP Manual on DateTime](https://www.php.net/manual/en/class.datetime.php)
- [PHP Manual on Date/Time Functions](https://www.php.net/manual/en/ref.datetime.php)
- [PHP The Right Way: Date and Time](https://phptherightway.com/#date_and_time)
