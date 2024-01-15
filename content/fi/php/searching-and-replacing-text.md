---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "PHP: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi etsiä ja korvata tekstiä? Yksi syy voi olla, että he haluavat nopeasti ja tehokkaasti muuttaa suuria määriä tekstiä sivustollaan tai sovelluksessaan ilman manuaalista työtä.

## Miten

Etsiminen ja korvaaminen tekstistä on helppoa PHP:llä. Sinun tarvitsee vain käyttää "preg_replace" -funktiota, joka etsii määrättyä tekstiä ja korvaa sen halutulla tekstillä. Katso alla oleva koodiesimerkki:

```
PHP
$text = "Tervetuloa maailmaan";
$new_text = preg_replace("/Tervetuloa/", "Welcome", $text);
echo $new_text;
// Output: Welcome maailmaan
```
Ensimmäisessä rivissä luodaan muuttuja "text", joka sisältää alkuperäisen tekstin. Toisessa rivissä käytetään "preg_replace" -funktiota, jossa ensimmäisessä parametrissa annetaan etsittävä teksti, toisessa parametrissa korvaava teksti ja kolmannessa parametrissa alkuperäinen teksti. Lopuksi koodi tulostaa uuden tekstin, jossa alkuperäinen teksti on korvattu halutulla tekstillä.

## Syvällinen sukellus

"Preg_replace" -funktion avulla voit myös käyttää säännöllisiä lausekkeita etsiessäsi tekstiä. Tämä antaa sinulle enemmän tarkkuutta ja joustavuutta korvataessa tekstiä. Voit esimerkiksi etsiä kaikki sanat, jotka alkavat "T" -kirjaimella ja korvata ne tekstillä "Hei". Katso alla oleva koodiesimerkki:

```
PHP
$text = "Tämä on vain esimerkki";
$new_text = preg_replace("/(^|\W)T(\w*)/", "$1Hei$2", $text);
echo $new_text;
// Output: Hei on vain esimerkki
```

Tässä koodissa käytetään säännöllistä lauseketta "/(^|\W)T(\w*)/" ensimmäisessä parametrissa, mikä merkitsee kaikkia sanoja, jotka alkavat "T" -kirjaimella. Sitten korvaavassa tekstissä käytetään merkkijonoja "$1" ja "$2", jotka tarkoittavat ensimmäistä ja toista osumaa säännöllisestä lausekkeesta. Tämä tarkoittaa, että alkuperäiset välimerkit ja seuraava kirjain ovat edelleen uudessa tekstissä.

## Katso myös

- [PHP: preg_replace docs](https://www.php.net/manual/en/function.preg-replace.php)
- [RegExp Tester](https://regex101.com/) - sivusto, jossa voit testata säännöllisiä lausekkeita ja niiden osumia.