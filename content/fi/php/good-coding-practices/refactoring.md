---
date: 2024-01-26 01:48:02.366899-07:00
description: "*Nimeksi annettu* kehitt\xE4minen on prosessi, jossa olemassa olevaa\
  \ tietokonekoodia uudelleenj\xE4rjestet\xE4\xE4n muuttamatta sen ulkoista k\xE4\
  ytt\xE4ytymist\xE4.\u2026"
lastmod: 2024-02-19 22:05:15.558684
model: gpt-4-0125-preview
summary: "*Nimeksi annettu* kehitt\xE4minen on prosessi, jossa olemassa olevaa tietokonekoodia\
  \ uudelleenj\xE4rjestet\xE4\xE4n muuttamatta sen ulkoista k\xE4ytt\xE4ytymist\xE4\
  .\u2026"
title: Koodin refaktorointi
---

{{< edit_this_page >}}

## Mikä & Miksi?
*Nimeksi annettu* kehittäminen on prosessi, jossa olemassa olevaa tietokonekoodia uudelleenjärjestetään muuttamatta sen ulkoista käyttäytymistä. Ohjelmoijat tekevät *nimeksi annettu-a* parantaakseen ohjelmiston ei-toiminnallisia ominaisuuksia, tehdensä koodista selkeämpää, tehokkaampaa ja helpommin ylläpidettävää.

## Kuinka:
Otetaan klassinen PHP-koodipätkä ja sovelletaan siihen *nimeksi annettu*-taikaa.

Ennen *nimeksi annettu-a*, koodimme saattaa näyttää tältä:

```php
function printOrderDetails($order) {
    foreach ($order as $item) {
        echo "Tuote: " . $item['name'];
        echo " - Hinta: " . $item['price'];
        echo "<br>";
    }
    
    if (!empty($order)) {
        echo "Yhteensä: " . array_sum(array_column($order, 'price'));
    }
}
```

Mutta voimme muokata tätä koodia parantaaksemme sen selkeyttä ja modulaarisuutta:

```php
function printItem($item) {
    echo "Tuote: {$item['name']} - Hinta: {$item['price']}<br>";
}

function calculateTotal($order) {
    return array_sum(array_column($order, 'price'));
}

function printOrderDetails(array $order) {
    array_walk($order, 'printItem');

    if (!empty($order)) {
        echo "Yhteensä: " . calculateTotal($order);
    }
}
```
Jakamalla `printOrderDetails` funktion pienempiin funktioihin, koodimme muuttuu luettavammaksi ja helpommaksi debugata.

## Syväsukellus
*Nimeksi annettu*-kehittäminen juontaa juurensa 1990-luvun alun smalltalk-ohjelmointiyhteisöstä ja Martin Fowler teki siitä suositumpaa teoksellaan "Refactoring: Improving the Design of Existing Code" (1999). Vaikka *nimeksi annettu*-kehittämistä voidaan soveltaa mihin tahansa ohjelmointikieleen, PHP:n dynaaminen luonne tarjoaa joitakin ainutlaatuisia haasteita ja mahdollisuuksia.

Vaihtoehtoja *nimeksi annettu-kehittämiselle* saattaa olla koodin kirjoittaminen alusta, mikä on usein riskialttiimpaa ja aikaavievämpää. PHP-ekosysteemissä, työkalut kuten PHPStan ja Rector voivat automaattisesti havaita ja suorittaa joitakin *nimeksi annettu*-operaatioita. Toteutuksen kannalta on avainasemassa pitää *nimeksi annettu*-muutokset pieninä ja testata laajasti yksikkötesteillä, jotta *nimeksi annettu*-kehittäminen onnistuu ilman bugien tuomista.

## Katso Myös
- Martin Fowlerin Refactoring-kirja: https://martinfowler.com/books/refactoring.html
- PHPStan, PHP:n staattinen analyysityökalu: https://phpstan.org/
- Rector, työkalu PHP-koodin automaattiseen *nimeksi annettu-kehittämiseen*: https://getrector.org/
- PHP-yksikkötestaus PHPUnitin avulla: https://phpunit.de/
