---
title:                "Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
html_title:           "PHP: Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
simple_title:         "Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Mikä & miksi?
Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä on yleinen tapaohjelmoijien käytössä. Se tarkoittaa tietyn ajanjakson lisäämistä tai vähentämistä nykyiseen päivämäärään. Tämä on hyödyllistä esimerkiksi laskutussovelluksissa, joissa tarvitaan tieto siitä, milloin tietty lasku tulee erääntyä tai vanhentua.

Miten:
PHP:llä tämä onnistuu helposti date() -funktion avulla. Tässä on yksinkertainen esimerkki lisäämällä 5 vuotta nykyiseen päivämäärään:

```PHP
$nykyinen_pvm = date('d.m.Y');
$uusi_pvm = date('d.m.Y', strtotime('+5 years'));
echo $uusi_pvm; // tulostaa päivämäärän 5 vuotta myöhemmältä
```

Tulostus: 26.03.2026

Deep Dive:
Historiallisesti päivämäärän laskeminen tulevaisuudessa tai menneisyydessä on ollut haastavaa. Ennen tietokoneiden aikaa se tehtiin manuaalisesti ja se vaati paljon laskemista ja päättelemistä. Nykyään se on huomattavasti helpompaa, ja siihen on saatavilla monia eri vaihtoehtoja kuin vain PHP-funktiot.

Mahdollisia vaihtoehtoja ovat esimerkiksi käyttää MySQL-tietokannan DATE_ADD tai DATE_SUB -funktioita, käyttää lisäosia, kuten Carbonia, tai jopa luoda oma päivämääränlaskemisfunktio käyttämällä esimerkiksi mktime() tai strtotime() -funktioita.

Lisäksi on hyvä huomioida aikavyöhykkeet ja kesäaikaan liittyvät muutokset, jotka voivat vaikuttaa päivämäärän laskemiseen.

Katso myös:
- PHP:n virallinen dokumentaatio date() -funktiosta: https://www.php.net/manual/en/function.date.php
- MySQL DATE_ADD ja DATE_SUB -funktiot: https://dev.mysql.com/doc/refman/8.0/en/date-and-time-functions.html#function_date-add
- Carbon-lisäosa: https://carbon.nesbot.com/
- mktime() -funktio: https://www.php.net/manual/en/function.mktime.php
- strtotime() -funktio: https://www.php.net/manual/en/function.strtotime.php