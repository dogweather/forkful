---
title:    "PHP: **Alimerkkijonojen erottaminen"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Miksi

Monet PHP-kehittäjät hyödyntävät substringsien "puuttumista" (engl. extraction) ohjelmointiessaan. Substringien poimiminen on kätevä tapa manipuloida merkkijonoja ja tehdä niistä tiettyjä toimintoja. Se voi myös auttaa koodin tehokkuudessa ja tietokannan käsittelyssä.

## Näin teet sen

Substringsien poimiminen PHP:lla on helppoa ja nopeaa. Tässä esimerkissä käytämme merkkijonoa "Tervetuloa Suomeen" ja poimimme siitä osan "Suomeen".

```PHP
$merkkijono = "Tervetuloa Suomeen";
echo substr($merkkijono, 11, 7);
```

Tämä tulostaa "Suomeen". Ensimmäisenä parametrina on merkkijono josta poimitaan, toisena parametrina aloituskohta ja kolmantena parametrina substringin pituus. Voit myös jättää viimeisen parametrin pois, jolloin substringin pituus lasketaan automaattisesti loppuun saakka.

Voit myös poimia merkkijonoja loppupäästä käyttämällä negatiivista aloituskohtaa. Esimerkiksi jos haluat poimia "Suomeen" merkkijonosta, voit käyttää seuraavaa koodia:

```PHP
$merkkijono = "Tervetuloa Suomeen";
echo substr($merkkijono, -7);
```

Tämä tulostaa saman "Suomeen" lopusta.

Voit myös poimia useamman kuin yhden substringsin käyttämällä pilkkuja merkkijonon sisällä. Esimerkiksi jos haluat poimia "Tervetuloa" ja "Suomeen" merkkijonosta, voit käyttää tätä:

```PHP
$merkkijono = "Tervetuloa Suomeen";
echo substr($merkkijono, 0, 10) . ", " . substr($merkkijono, 11);
```

Tämä tulostaa "Tervetuloa, Suomeen".

## Syvemmälle

Substringsien poimiminen ei rajoitu vain merkkijonoihin, vaan se toimii myös taulukoissa ja assosiatiivisissa taulukoissa. Voit käyttää samaa substr-funktiota, mutta sen ensimmäisenä parametrinä on taulukko ja sen toisena parametrina numerollinen tai merkkijonona oleva avain.

```PHP
$taulukko = array("Tervetuloa", "Suomeen");
echo substr($taulukko[1], 0, 5);
```

Tämä tulostaa "Suome". Voit myös poimia substringsin assosiatiivisesta taulukosta käyttäen sen avainlukua.

```PHP
$taulukko = array("nimi" => "Suomeen", "tervehdys" => "Tervetuloa");
echo substr($taulukko["nimi"], 0, 5);
```

Tämä tulostaa "Suome".

## Katso myös

- PHP-käsikirja substr-funktiolle: https://www.php.net/manual/en/function.substr.php
- W3Schools substr-esimerkkejä: https://www.w3schools.com/php/func_string_substr.asp
- Live Coding -video substringien käytöstä: https://www.youtube.com/watch?v=Uc03GYl4PZo