---
date: 2024-01-20 17:47:50.925638-07:00
description: "How to: / Kuinka: PHP:ss\xE4 merkkijonon pituuden saat selvitetty\xE4\
  \ `strlen`-funktiolla. T\xE4ss\xE4 lyhyt esimerkki."
lastmod: '2024-04-05T22:38:57.256124-06:00'
model: gpt-4-1106-preview
summary: "/ Kuinka: PHP:ss\xE4 merkkijonon pituuden saat selvitetty\xE4 `strlen`-funktiolla.\
  \ T\xE4ss\xE4 lyhyt esimerkki."
title: "Merkkijonon pituuden selvitt\xE4minen"
weight: 7
---

## How to: / Kuinka:
PHP:ssä merkkijonon pituuden saat selvitettyä `strlen`-funktiolla. Tässä lyhyt esimerkki:

```php
<?php
$esimerkki = "Moi maailma!";
$pituus = strlen($esimerkki);
echo $pituus; // Tulostaa: 12
?>
```

Jos käsittelet monitavuisia merkkijonoja, esim. UTF-8-koodattuja, käytä `mb_strlen`:

```php
<?php
$esimerkki = "Hei maailma!";
$pituus = mb_strlen($esimerkki, 'UTF-8');
echo $pituus; // Tulostaa: 12, mutta ottaa huomioon monitavuiset merkit
?>
```

## Deep Dive / Syväsukellus:
Aikoinaan, kun enemmistö merkkijonosta käytti yhden tavun merkistöjä (kuten ASCII), `strlen` oli suora ja nopea tapa selvittää merkkijonon pituus. Kuitenkin, kun kansainvälistyminen ja monitavuiset merkkistöt yleistyivät, syntyi tarve kuten `mb_strlen`, joka tukee monenlaisia merkistökoodauksia.

Vaihtoehto `strlen`-funktiolle on esimerkiksi `graf_strlen()`, joka laskee grafeemit (näkyvät merkkiyhdistelmät) Unicode merkkijonossa, joka on hyödyllinen joissain erittäin erikoistuneissa tapauksissa.

Toteutusyksityiskohtana, PHP:n sisäinen `strlen` on toteutettu C-tasolla ja on hyvin optimoitu. `mb_strlen`, vaikka hitaampi kuin `strlen` tavumerkkijonoille, on kriittinen, kun työskentelet UTF-8 merkkijonojen kanssa, jotta voit kunnolla käsittellä merkkijonon pituutta.

## See Also / Katso Myös:
- PHP Manual strlen: [https://www.php.net/manual/en/function.strlen.php](https://www.php.net/manual/en/function.strlen.php)
- PHP Manual mb_strlen: [https://www.php.net/manual/en/function.mb-strlen.php](https://www.php.net/manual/en/function.mb-strlen.php)
- Unicode ja PHP: [https://www.php.net/manual/en/book.mbstring.php](https://www.php.net/manual/en/book.mbstring.php)
