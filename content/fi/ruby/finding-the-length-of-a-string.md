---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Ruby: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi löytää merkkijonon pituuden? Yksinkertaisesti sanottuna, merkkijonon pituuden löytäminen on tärkeää tietoa ohjelmoinnissa ja auttaa varmistamaan, että koodi toimii odotetulla tavalla.

## Kuinka

Merkkijonon pituuden löytäminen Rubyssa on helppoa käyttämällä `length`-metodia. Seuraavassa esimerkissä käytämme `length`-metodia löytääksemme merkkijonon pituuden ja tulostamme sen konsoliin:

```Ruby
string = "Hei maailma!"
puts string.length
```

Tämä koodi tulostaa "12", mikä on merkkijonon "Hei maailma!" pituus.

Voimme myös käyttää `size`-metodia saadaksemme saman tuloksen:

```Ruby
string = "Hei maailma!"
puts string.size
```

Molemmat metodit toimivat samalla tavalla ja palauttavat merkkijonon pituuden.

## Syväsukellus

Merkkijonojen pituuden löytäminen perustuu merkkien lukumäärän laskemiseen. Rubyssa, jokainen merkkiä vastaa yksi merkki. Tämä tarkoittaa, että erikoismerkit, kuten välilyönnit tai aksenttimerkit, lasketaan osaksi merkkijonon pituutta.

Voimme myös käyttää `length`-metodia muilla tietotyypeillä, kuten taulukoilla tai hajautustauluilla. Taulukoiden tapauksessa metodi palauttaa taulukon pituuden, eli sen sisältämien elementtien määrän.

On myös tärkeää huomata, että merkkijonon pituuden löytämisestä ei ole suoranaista hyötyä, jos käytämme vain kovakoodattuja merkkijonoja. Mutta jos käytämme muuttujia ja käyttäjän syötteitä, voimme käyttää tätä tietoa ohjelman toiminnan perustana.

## Katso myös

- [Ruby String -dokumentaatio](https://ruby-doc.org/core-2.7.2/String.html)
- [Ruby Array -dokumentaatio](https://ruby-doc.org/core-2.7.2/Array.html)
- [Ruby Hash -dokumentaatio](https://ruby-doc.org/core-2.7.2/Hash.html)