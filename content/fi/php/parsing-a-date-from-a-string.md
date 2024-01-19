---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "Bash: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# PHP:n päivämäärien jäsennys stringeistä: Mitä & Miksi?

Päivämäärän jäsentäminen stringistä on prosessi, jossa datan alustava muoto - string - muunnetaan päivämääräksi, jolla on järkevä, ymmärrettävä muoto. Ohjelmoijat tekevät sen tietojen manipuloinnin helpottamiseksi.

# PHP:n päivämäärien jäsennys stringeistä: Miten se tehdään?

Alla on esimerkki siitä, miten PHP:ssa jäsennetään päivämäärä stringistä `strtotime()`-funktion avulla:

```PHP
<?php
$date_string = "2020-04-01";
$parsed_date = date("d-m-Y", strtotime($date_string));
echo $parsed_date;
?>
```

Tämä skripti tulostaa `01-04-2020`.

# PHP:n päivämäärien jäsennys stringeistä: Syvä sukellus

Historiallisessa kontekstissa PHP tarjosi `strtotime()`-funktion syntaksin ymmärtämiseen ja päivämäärän jäsentämiseen stringistä. Tämä tehtiin sen avulla - mikä ilmestyi jo PHP 1:ssä.

Vaihtoehtoja `strtotime()`-funktiolle ovat DateTime-luokka ja `date_create_from_format()`-funktio. Näiden avulla voidaan jäsennellä päivämäärä erityismuodoista, jotka `strtotime()` ei ymmärrä.

`strtotime()` käsittelee päivämäärien jäsentämisen stringin internalisoimalla sen sisäiseksi päivämääräksi. Tämä sallii monimutkaisten päivämäärien ja aikojen käsittelyn.

# PHP:n päivämäärien jäsennys stringeistä: Katso myös

Lisätietoja päivämäärien jäsentämisen haasteista ja muista PHP-aiheista:

- [PHP Manual: Date and Time Functions](https://www.php.net/manual/en/ref.datetime.php)
- [DateTime::createFromFormat](https://www.php.net/manual/en/datetime.createfromformat.php)
- [Understanding Date and Time in PHP](https://www.w3schools.com/php/php_date.asp)