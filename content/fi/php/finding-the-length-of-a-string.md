---
title:    "PHP: Merkkijonon pituuden löytäminen"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Miksi

Miksi sinun pitäisi vaivautua etsimään merkkijonon pituus? Tämä taito voi olla erittäin hyödyllinen, kun työskentelet esimerkiksi lomakkeiden kanssa ja haluat tarkistaa, että käyttäjän syöttämät tiedot eivät ylitä tiettyä rajoitusta.

# Miten

Merkkijonon pituuden löytäminen PHP: ssä on helppoa. Voit käyttää valmiiksi rakennettua `strlen ()` -funktiota, joka palauttaa merkkijonon pituuden.

```PHP
<?php
$string = "Tervetuloa PHP-ohjelmoinnin maailmaan!";
echo strlen($string); // tulostaa 36
?>
```

Tässä esimerkissä laskemme merkkijonon "Tervetuloa PHP-ohjelmoinnin maailmaan!" pituuden ja tulostamme sen. Huomaa, että välilyönnit lasketaan myös merkkijonon pituuteen.

# Syvällinen sukellus

Merkkijonon pituuden löytämiseen liittyy muutamia tärkeitä yksityiskohtia. Ensinnäkin, `strlen ()` -funktion palauttama arvo on sama kuin kaikkien merkkien lukumäärä merkkijonossa, mukaan lukien välilyönnit ja erikoismerkit.

Toiseksi, jos käytät monikielisiä merkkijonoja, huomioi että tietyt merkit voivat vaatia enemmän tavuja kuin toiset. Tällöin merkkijonon pituuden laskeminen ei ole yksinkertaista `strlen ()` -funktion avulla.

Lopuksi, on hyvä muistaa, että merkkijonon pituus vaihtelee eri ohjelmointikielillä. Joten, jos käytät useita ohjelmointikieliä samassa projektissa, tarkista jokaisen kielen oma tapa laskea merkkijonon pituus.

# Katso myös

- [PHP-dokumentaatio merkkijonon pituuden laskemisesta](https://www.php.net/manual/en/function.strlen.php)
- [W3Schools-opetusohjelma merkkijonon pituuden löytämisestä PHP: ssä](https://www.w3schools.com/php/func_string_strlen.asp)
- [10 tapaa mitata merkkijonon pituutta PHP: ssä](https://www.tutorialrepublic.com/faq/how-to-find-the-length-of-a-string-in-php.php)