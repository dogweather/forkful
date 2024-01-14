---
title:    "Bash: Merkkijonon pituuden löytäminen"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi haluat selvittää merkkijonon pituuden Bash-ohjelmointikielen avulla. Ehkä haluat tarkistaa, onko merkkijono tarpeeksi pitkä ennen sen käyttöä, taikka ehkä tarvitset pituustiedon lausekkeissa ja pylväskaavioissa. Ei ole väliä miksi, kunhan tiedät, että Bash tarjoaa helpon tavan löytää merkkijonon pituuden.

## Miten

Bashissa voit käyttää sisäänrakennettua `wc` komentoa löytääksesi merkkijonon pituuden. Se ottaa `-l` argumentin ja tulostaa rivien määrän, mikä vastaa merkkijonon pituutta. Katso esimerkki alla:

```Bash
string="Hei maailma!"
length=$(echo $string | wc -l)
echo "Merkkijonon pituus on $length"
```

Tämä koodi tulostaisi "Merkkijonon pituus on 13". Voit myös käyttää `wc -c` komentoa saadaksesi merkkien määrän merkkijonossa.

## Syvempi sukellus

Tiesitkö, että voit myös käyttää sisäänrakennettua `expr` komentoa löytääksesi merkkijonon pituuden? Se ottaa `-length` argumentin ja laskee merkkien määrän merkkijonossa. Katso esimerkki alla:

```Bash
string="Tervetuloa"
length=$(expr length $string)
echo "Merkkijonon pituus on $length"
```

Tämä koodi tulostaisi myös "Merkkijonon pituus on 10". Valitettavasti `expr` ei näytä olevan yhtä tarkka kuin `wc` - joten jos merkkijonossa on erikoismerkkejä, voit saada erilaiset tulokset.

## Katso myös

- [Bashin `wc` manuaalisivu](https://www.gnu.org/software/coreutils/manual/html_node/wc-invocation.html)
- [Bashin `expr` manuaalisivu](https://www.gnu.org/software/coreutils/manual/html_node/expr-invocation.html)