---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
html_title:           "PHP: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Yksi yleinen toimenpide PHP-ohjelmoinnissa on merkkijonojen muokkaaminen. Toisinaan haluamme esimerkiksi muuttaa merkkijonon ensimmäisen kirjaimen isoksi. Tämä voi olla hyödyllistä esimerkiksi, kun haluat näyttää käyttäjän syötteet oikeassa muodossa.

## Kuinka tehdä

```PHP
$string = "tämä on esimerkki";
echo ucfirst($string);
```

Tulostus:
```
Tämä on esimerkki
```

Funktio `ucfirst()` muuttaa merkkijonon ensimmäisen kirjaimen isoksi. Tämä koskee kuitenkin vain ensimmäistä kirjainta eikä vaikuta muihin kirjaimiin merkkijonon sisällä.

Voimme myös muuttaa kaikki merkkijonon kirjaimet isoksi käyttämällä funktiota `strtoupper()`:

```PHP
$string = "tämä on esimerkki";
echo strtoupper($string);
```

Tulostus:
```
TÄMÄ ON ESIMERKKI
```

## Syvempi sukellus

Merkkijonon muokkaamiseen on muitakin tapoja kuin vain muuttaa ensimmäinen kirjain isoksi. Voimme käyttää esimerkiksi funktioita `strtolower()` ja `ucwords()` muuttaaksemme kaikki kirjaimet pieniksi tai muuttaaksemme pelkän ensimmäisen kirjaimen jokaisessa sanassa isoksi.

Lisäksi merkkijonoihin on mahdollista käyttää monia muita manipulointifunktioita, kuten `trim()` poistaaksemme tyhjät välilyönnit merkkijonon alusta ja lopusta.

## Katso myös

- [PHP:n virallinen dokumentaatio merkkijonojen muokkaamisesta](https://www.php.net/manual/en/ref.strings.php)
- [Tutoriaali merkkijonojen muokkaamisesta PHP:llä](https://www.w3schools.com/php/php_string.asp)
- [Konversiokytkimet ja merkkijonon muotoilu PHP:ssä](https://www.freecodecamp.org/news/a-quick-intro-to-formatting-strings-in-php-sprintf-strftime-and-more/)