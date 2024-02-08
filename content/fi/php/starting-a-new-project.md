---
title:                "Uuden projektin aloittaminen"
aliases:
- fi/php/starting-a-new-project.md
date:                  2024-01-20T18:04:43.108160-07:00
model:                 gpt-4-1106-preview
simple_title:         "Uuden projektin aloittaminen"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Uuden projektin aloittaminen PHP:ssä on kuin tyhjältä pöydältä aloittamista; uutta luovaa prosessia. Koodarit tekevät sen paketoidakseen ideoita ja ratkaistakseen ongelmia.

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
