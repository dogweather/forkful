---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Bash: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Oletko koskaan tarvinnut selvittää merkkijonon pituutta Bash-ohjelmoinnissa? Ehkä haluat tulostaa vain enintään tietyn määrän merkkejä tai tehdä tarkistuksen, jos merkkijono on tarpeeksi lyhyt tai pitkä. Tässä artikkelissa opit, kuinka löydät merkkijonon pituuden Bashin avulla.

## Kuinka tehdä

Ensinnäkin, haluat varmistaa, että merkkijono, jonka pituuden haluat selvittää, on tallennettu muuttujaan. Voit tehdä tämän käyttämällä seuraavaa syntaksia:

```Bash
string="Tämä on esimerkki merkkijono"
```

Sitten voit käyttää `expr`-komennolla saadaksesi merkkijonon pituuden:

```Bash
length=$(expr length $string)
```

Viimeisessä komennossa luodaan uusi muuttuja, `length`, joka tallentaa merkkijonon pituuden. Tämän muuttujan voit sitten tulostaa komennolla `echo`:

```Bash
echo "Merkkijonon pituus on $length merkkiä."
```

Tässä on esimerkkituloste:

```Bash
Merkkijonon pituus on 27 merkkiä.
```

## Syvempi sukellus

Bashissa on myös muita tapoja löytää merkkijonon pituus, kuten käyttämällä `length`-ominaisuutta tai `wc`-komennolla. Voit myös käyttää `cut`-komennolla saadaksesi halutun määrän merkkejä merkkijonosta. Lisätietoja näistä vaihtoehdoista löydät Bashin dokumentaatiosta.

Lisäksi voit saada merkkijonon pituuden ilman ulkoista komentoa käyttämällä Bashin sisäänrakennettuja ominaisuuksia. Esimerkiksi voit käyttää `${#muuttuja}` syntaksia saadaksesi muuttujan pituuden suoraan ilman lisäkomentoja.

## Katso myös

- [Bashin viralliset dokumentaatiot](https://www.gnu.org/software/bash/manual/)
- [Bashin muuttujien käsittely](https://www.tutorialspoint.com/unix/unix-shell-variable.htm)
- [Bash-komentojen käyttö](https://www.shellscript.sh/variables1.html)