---
title:                "C: Merkkijonon pituuden selvittäminen"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Miksi?

Koodin pituuden löytäminen saattaa tuntua yksinkertaiselta tehtävältä, mutta se on yllättävän tärkeä taito ohjelmoijalle. Kun tiedät kuinka monta merkkiä koodisi sisältää, voit esimerkiksi varmistaa, että käytät oikean kokoista muistia tai että et ylitä merkki- tai sanarajoja.

# Kuinka saada selville?

Voit selvittää merkkijonon pituuden käyttämällä C:n sisäänrakennettua funktiota `strlen()`. Tämä funktio ottaa parametrinaan merkkijonon ja palauttaa sen pituuden kokonaislukuna. Alla on kaksi yksinkertaista esimerkkiä sen käytöstä:

```C
char str[] = "Tämä on merkkijono"; 
int length = strlen(str); 
printf("Merkkijonon pituus on: %d\n", length); 
```

Tämä tulostaa: `Merkkijonon pituus on: 19`.

```C
char* str = "Toinen merkkijono"; 
int length = strlen(str); 
printf("Merkkijonon pituus on: %d\n", length); 
```

Tämä tulostaa: `Merkkijonon pituus on: 18`.

# Syvemmälle asiassa

Tämä funktio käyttää C:n nollapäätöslaskentaa määrittämään merkkijonon pituuden. Se käy läpi merkkijonon merkkejä, kunnes se saavuttaa nollamerkin, joka ilmaisee merkkijonon lopun. Tästä syystä `strlen()` ei ota huomioon nollamerkkiä, vaan laskee vain merkkien määrän ilman sitä.

On myös tärkeää muistaa, että `strlen()` laskee merkkien määrän vain silloin, kun merkkijono on määritetty tavallisena c-merkkilistana. Jos merkkijono on esimerkiksi tallennettu osoittimena (`char*`), `strlen()` ei välttämättä toimi oikein.

# Katso myös

- [C:n strlen() funktio dokumentaatio](https://www.cplusplus.com/reference/cstring/strlen/)
- [C:n merkkilistat ja nollapäätöslaskenta](https://www.geeksforgeeks.org/character-array-vs-string-c/)
- [Tietoa merkkijonoista ja niiden manipuloinnista C-kielessä](https://www.tutorialspoint.com/cprogramming/c_strings.htm)