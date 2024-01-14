---
title:                "PHP: Debug-tulostuksen tulostaminen"
simple_title:         "Debug-tulostuksen tulostaminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Jokainen ohjelmoija törmää tilanteisiin, joissa koodin toimintaa täytyy tutkia tarkemmin. Tässä tilanteessa debug-tulostuksen avulla pystytään helposti havainnollistamaan koodin toimintaa ja löytämään mahdolliset virheet.

## Kuinka tehdä

Debug-tulostusta varten voidaan käyttää PHP:n sisäänrakennettuja funktioita kuten "var_dump" tai "print_r". Näitä funktioita voidaan käyttää tulostamaan tietoa muuttujista ja niiden arvoista:

```PHP
$numero = 5;
var_dump($numero);
// Outputs: int(5)

$lista = array("omena", "banaani", "mango");
print_r($lista);
// Outputs: Array ( [0] => omena [1] => banaani [2] => mango )
```

Voit myös käyttää debug-tulostusta tarkemmassa kontekstissa ottamalla huomioon koodin suoritusjärjestyksen. Esimerkiksi "exit" -funktiota voidaan käyttää tarkistamaan, että ohjelma suorittaa halutut toiminnot halutussa järjestyksessä:

```PHP
$num1 = 10;
$num2 = 20;
exit("Numeroiden summa on: " . ($num1 + $num2));
// Outputs: Numeroiden summa on: 30
```

## Syväsukellus

Kun olet tutustunut debug-tulostuksen perusteisiin, voit alkaa hyödyntää sitä monipuolisemmin. Voit esimerkiksi tulostaa tietoa objekteista ja niiden ominaisuuksista:

```PHP
class Auto {
    public $merkki;
    public $malli;
    public $vuosimalli;

    public function __construct($merkki, $malli, $vuosimalli) {
        $this->merkki = $merkki;
        $this->malli = $malli;
        $this->vuosimalli = $vuosimalli;
    }
}

$auto = new Auto("Audi", "A4", 2019);
var_dump($auto);
// Outputs: class Auto#1 (3) {
//             public $merkki => string(4) "Audi"
//             public $malli => string(2) "A4"
//             public $vuosimalli => int(2019)
//         }
```

## Katso myös

- [PHP:n varsinainen debuggaus-sivu](https://www.php.net/manual/en/debugger.php)
- [Laravel Debugbar -työkalu](https://github.com/barryvdh/laravel-debugbar)
- [Debugging in PHP -video-opas](https://www.youtube.com/watch?v=3BRImzGdQO0&t=5s)