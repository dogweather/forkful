---
title:    "Bash: Merkkijonon pituuden löytäminen"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa saattaa olla tarpeen selvittää merkkijonon pituus Bash-ohjelmointikielessä. Esimerkiksi jos haluat tarkistaa, että käyttäjän antama syöte on tietyllä määräämälläsi merkin pituudella tai haluat yksinkertaisesti näyttää käyttäjälle, kuinka monta merkkiä hänen antamassaan sanassa on. Näissä tapauksissa on hyödyllistä tietää, kuinka löytää merkkijonon pituus Bashissa.

## Miten

Merkkijonon pituuden löytäminen Bashissa on hyvin yksinkertaista. Voit käyttää `expr length` -komentoa ja antaa sille merkkijonon, jonka haluat tutkia. Esimerkiksi:

```Bash
expr length "Tämä on kirjaimellisesti vain esimerkki"
```
Tulosteena saamme:

```Bash
38
```
Huomaa, että välilyönnit lasketaan myös merkeiksi merkkijonon pituutta laskettaessa. Voit myös käyttää `wc -m` -komentoa, joka laskee merkkien määrän. Esimerkiksi:

```Bash
echo -n "Tämä on toinen esimerkki" | wc -m
```

Tämä tulostaa:

```Bash
25
```

Kuten näemme, `wc -m` -komento ei laske välilyöntejä merkkeihin. Se on siis hieman tarkempi vaihtoehto, jos haluat tarkistaa vain varsinaisen merkkien määrän.

## Syvempi sukellus

Bashissa on myös muita tapoja löytää merkkijonon pituus, kuten käyttämällä sisäänrakennettua `length` -muuttujaa. Esimerkiksi:

```Bash
myString="Tämä on vielä yksi esimerkki"
echo ${#myString}
```
Tämä tulostaa:

```Bash
27
```

Huomaa, että tässä tapauksessa tarvitaan myös `#` -merkki ennen muuttujan nimeä.

Voit myös käyttää komentoa `printf`, joka antaa mahdollisuuden tehdä enemmän muokkauksia tulosteeseen. Esimerkiksi:

```Bash
myString="Ja viimeisenä esimerkki"
printf '%s' "${#myString}"
```
Tämä tulostaa:

```Bash
23
```

## Katso myös

- `expr length` -komento: https://www.gnu.org/software/coreutils/manual/html_node/expr-invocation.html
- `wc -m` -komento: https://www.gnu.org/software/coreutils/manual/html_node/wc-invocation.html
- Muuttujien käyttäminen Bashissa: https://wiki.bash-hackers.org/syntax/pe