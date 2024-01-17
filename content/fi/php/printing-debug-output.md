---
title:                "Virheenkorjaustulosteen tulostaminen"
html_title:           "PHP: Virheenkorjaustulosteen tulostaminen"
simple_title:         "Virheenkorjaustulosteen tulostaminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?

Tulostaminen debug-tulostetta on tapa, jolla voit tarkastella ohjelman suorituskykyä ja virheitä koodin suorittamisen aikana. Useimmat ohjelmoijat käyttävät tätä tekniikkaa, koska se auttaa heitä tunnistamaan ja korjaamaan virheitä nopeammin ja tehokkaammin.

## Kuinka:

```php
// Tulosta tekstiä konsoliin
echo "Debug-tulostus on päällä!";

// Tulosta muuttujan arvo
$muuttuja = "Tulosta tämä!";
echo $muuttuja;

// Tulosta taulukon sisällöt
$taulukko = array("yksi", "kaksi", "kolme");
print_r($taulukko);

// Tulosta olio
$laskuri = new Laskuri();
var_dump($laskuri);
```

Esimerkkilähtö:

```
Debug-tulostus on päällä!
Tulosta tämä!
Array
(
    [0] => yksi
    [1] => kaksi
    [2] => kolme
)
object(Laskuri)#1 (0) {
}
```

## Syvään kaivautuminen

Debug-tulostus on ollut osa PHP:ta lähes sen alkuajoista lähtien ja sitä käytetään edelleen yhtenä tärkeimmistä työkaluista virheiden löytämiseen ja korjaamiseen ohjelmointivaiheessa. Sitä voidaan käyttää myös apuna koodin tarkastelussa ja optimoinnissa. Vaihtoehtoisesti voit myös käyttää erillisiä debuggaustyökaluja, kuten Xdebug.

## Katso myös

- https://www.php.net/manual/en/function.print-r.php
- https://www.php.net/manual/en/function.var-dump.php
- https://xdebug.org/