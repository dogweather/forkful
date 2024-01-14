---
title:    "PHP: Komentoriviparametrien lukeminen"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Miksi?

Tervetuloa taas blogiini, rakkaat lukijat! Tässä artikkelissa syvennymme PHP-ohjelmointiin ja kerron teille, miksi on tärkeää lukea komentorivien argumentteja koodia kirjoittaessa.

Komentorivit ovat välttämätön osa monia ohjelmia ja skriptejä. Ne antavat käyttäjille mahdollisuuden ohjata ohjelman toimintaa syöttämällä erilaisia parametreja, kuten tiedoston nimiä tai vaihtoehtoisia asetuksia. Näitä komentoja voidaan myös käyttää kehittäjien tai testaajien toimesta ohjelman toiminnan tarkasteluun ja muokkaukseen. Joten on tärkeää, että osaamme lukea näitä komentorivien argumentteja koodissamme.

## Miten?

PHP:ssa on valmiita funktioita komentorivin argumenttien lukemiseen. Lähestymistapa riippuu kuitenkin siitä, miten argumentit on syötetty. Jos ne annetaan peräkkäin, voidaan käyttää `getopt()`-funktiota. Tässä esimerkki:

```PHP
<?php

$options = getopt("f:l:");

if (isset($options['f'])) {
    // Tulostaa ensimmäisen argumentin arvon
    echo "Ensimmäinen argumentti: " .$options['f'] ."\n";
}

if (isset($options['l'])) {
    // Tulostaa toisen argumentin arvon
    echo "Toinen argumentti: " .$options['l'] ."\n";
}
```

Ajamalla tätä skriptiä komentoriviltä komennolla `php script.php -f Hello -l World` tulostuu seuraava:

```
Ensimmäinen argumentti: Hello
Toinen argumentti: World
```

Jos argumentit annetaan peräkkäin eli `php script.php Hello World`, voidaan käyttää `$_SERVER['argv']`-muuttujaa. Tässä esimerkki:

```PHP
<?php

if (isset($_SERVER['argv'][1])) {
    // Tulostaa ensimmäisen argumentin arvon
    echo "Ensimmäinen argumentti: " .$_SERVER['argv'][1] ."\n";
}

if (isset($_SERVER['argv'][2])) {
    // Tulostaa toisen argumentin arvon
    echo "Toinen argumentti: " .$_SERVER['argv'][2] ."\n";
}
```

Ajamalla tätä skriptiä komentoriviltä komennolla `php script.php Hello World` tulostuu seuraava:

```
Ensimmäinen argumentti: Hello
Toinen argumentti: World
```

## Syvällinen tarkastelu

Komentorivien argumenttien lukeminen PHP:ssa voi olla monimutkaista, jos argumentit sisältävät välilyöntejä tai muita erikoismerkkejä. Silloin tulee käyttää `escapeshellarg()`-funktiota jokaiselle argumentille, jotta ne voidaan käsitellä oikein. Tässä esimerkki:

```PHP
<?php

$options = getopt("f:l:");

if (isset($options['f'])) {
    // Tulostaa ensimmäisen argumentin arvon
    echo "Ensimmäinen argumentti: " .escapeshellarg($options['f']) ."\n";
}

if (isset($options['l'])) {
    // Tulostaa toisen argumentin arvon
    echo "Toinen argumentti: " .escapeshellarg($options['l']) ."\n";
}
```

Ajamalla tätä skriptiä komentoriviltä komennolla `php script.php -f "Hello World" -l "Hello, World!"` tulostuu seuraava:

```
Ensimmäinen argumentti: 'Hello World '
Toinen argumentti: 'Hello, World!'
```

Yllä käytetään myös vaihtoehtoa `f:l:`, joka tarkoittaa, että funkt