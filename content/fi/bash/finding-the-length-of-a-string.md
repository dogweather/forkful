---
title:                "Bash: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi
Miksi haluaisit selvittää merkkijonon pituuden? Yksi syy voisi olla, että haluat tietää kuinka monta merkkiä merkkijonossa on tai haluat suorittaa tarkistuksia tiettyjen merkkien määrään perustuen.

## Kuinka tehdä se
Voit helposti löytää merkkijonon pituuden Bash-skriptillä käyttämällä `expr length` -komentoa. Katso esimerkki alla:

```Bash
#!/bin/bash
merkkijono="Tämä on esimerkki merkkijonosta."
merkkijonon_pituus=`expr length "$merkkijono"`
echo "Merkkijonon \"$merkkijono\" pituus on $merkkijonon_pituus merkkiä."
```

Tämän skriptin suoritus antaa seuraavan tulosteen:

```Bash
Merkkijonon "Tämä on esimerkki merkkijonosta." pituus on 30 merkkiä.
```

## Syvällisempi sukellus
`expr length` -komennolla voit myös laskea suoraan minkä tahansa muuttujan pituuden. Voit myös käyttää muita argumentteja, kuten `substr`, jolla voit erottaa tietyn alueen merkkijonosta ja laskea sen pituuden. Tarkempia tietoja saat Bashin dokumentaatiosta.

See also
- [Bashin dokumentaatio](https://www.gnu.org/software/bash/manual/html_node/)
- [Tietoja Rope String -ratkaisuohjelmasta](https://ropestring.com/)
- [Merkkijonon pituuden laskeminen Pythonissa -blogikirjoitus](https://djangocentral.com/find-the-length-of-a-string-in-python/)