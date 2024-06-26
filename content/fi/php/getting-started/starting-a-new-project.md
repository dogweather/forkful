---
date: 2024-01-20 18:04:43.108160-07:00
description: "How to: (Kuinka tehd\xE4:) Aloitetaan yksinkertaisella `\"Hello, world!\"\
  ` -esimerkill\xE4. Tallenna t\xE4m\xE4 tiedostoksi nimelt\xE4 `index.php`."
lastmod: '2024-04-05T21:53:58.228588-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) Aloitetaan yksinkertaisella `\"Hello, world!\"` -esimerkill\xE4\
  ."
title: Uuden projektin aloittaminen
weight: 1
---

## How to: (Kuinka tehdä:)
Aloitetaan yksinkertaisella `"Hello, world!"` -esimerkillä. Tallenna tämä tiedostoksi nimeltä `index.php`.

```php
<?php
echo "Hello, world!";
?>
```

Aja tämä paikallisella palvelimellasi ja näet tulosteen:

```
Hello, world!
```

Sitten, luodaan yksinkertainen lomake käyttäjän syötteen keräämiseksi. Tiedosto `form.php`:

```php
<?php
if ($_SERVER['REQUEST_METHOD'] === 'POST') {
    $name = $_POST['name'] ?? 'nobody';
    echo "Hello, {$name}!";
    exit;
}
?>

<form method="POST">
    <label for="name">Nimesi:</label>
    <input type="text" id="name" name="name">
    <button type="submit">Lähetä</button>
</form>
```

Tämä tulostaa `Hello, [nimi]!`, kun lomake lähetetään.

## Deep Dive (Sukellus syvyyksiin)
Aloitettaessa uuden PHP-projektin tekemistä on hyvä ymmärtää kontekstia. PHP syntyi 90-luvulla yksinkertaisena skriptaustyökaluna, mutta on kasvanut täydeksi ohjelmointikieleksi, joka pyörittää isoa osaa internetistä. Nykyään on monia vaihtoehtoja uuden projektin aloittamiseen – raamit, kuten Laravel tai Symfony, jotka tarjoavat rikkaan toiminnallisuuden ja ovat ajan tasalla parhaiden käytäntöjen kanssa. 

Implementoidessa alusta asti, on tärkeää valita sopiva tiedostorakenne. Pidä sovelluslogiikka, näkymät ja muut resurssit erillään. Käytä Composeria riippuvuuksien hallintaan. Noudattamalla PSR-standardeja varmistat koodisi laadun ja ylläpidettävyyden. Aloita pienesti, suunnittele eteenpäin.

## See Also (Katso myös)
- PHP:n viralliset dokumentit: [php.net/manual](https://www.php.net/manual/en/)
- Composer, riippuvuuksien hallintatyökalu: [getcomposer.org](https://getcomposer.org/)
- PHP-FIG ja PSR-standardit: [php-fig.org/psr/](https://www.php-fig.org/psr/)
- Framework-katsaukset: Laravel [laravel.com](https://laravel.com/) ja Symfony [symfony.com](https://symfony.com/)
