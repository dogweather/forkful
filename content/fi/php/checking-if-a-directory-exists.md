---
title:    "PHP: Tarkistetaan, onko hakemisto olemassa."
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi
On monia tilanteita, joissa on tarpeellista tarkistaa, onko hakemistoa olemassa. Tämä voi auttaa estämään virheitä koodissa ja lisätä sovelluksen turvallisuutta.

## Miten
PHP:lla on useita tapoja tarkistaa hakemiston olemassaoloa. Yksi tapa on käyttää `file_exists()` -funktiota, joka palauttaa `true` tai `false` riippuen siitä, löytyykö hakemisto annetusta polusta. Esimerkiksi:

```PHP
if(file_exists("/polku/hakemistoon")) {
  echo "Hakemisto löytyy!";
} else {
  echo "Hakemistoa ei löydy.";
}
```

Toinen tapa on käyttää `is_dir()` -funktiota, joka tarkistaa, onko annettu polku hakemisto vai ei. Esimerkiksi:

```PHP
if(is_dir("/polku/hakemistoon")) {
  echo "Kyseessä on hakemisto!";
} else {
  echo "Kyseessä ei ole hakemisto.";
}
```

## Syventävä tarkastelu
On myös hyvä huomata, että nämä funktiot voivat palauttaa virheilmoituksia, jos niille annetaan vääränlainen polku tai jos hakemistoa ei ole oikeuksia lukea. Tästä syystä on tärkeää käsitellä näitä virhetilanteita koodissa.

Lisäksi, jos tarkistat hakemiston olemassaoloa ennen sen luomista, voit käyttää `mkdir()` -funktiota luodaksesi hakemiston vain, jos sitä ei jo ole. Näin voit välttää hakemiston uudelleen luomisen ja virheilmoitusten aiheuttamia häiriöitä.

## Katso myös
- [file_exists()-funktio PHP:n virallisessa dokumentaatiossa](https://www.php.net/manual/en/function.file-exists.php)
- [is_dir()-funktio PHP:n virallisessa dokumentaatiossa](https://www.php.net/manual/en/function.is-dir.php)
- [mkdir()-funktio PHP:n virallisessa dokumentaatiossa](https://www.php.net/manual/en/function.mkdir.php)