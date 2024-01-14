---
title:                "Ruby: Perhetien pituuden löytäminen"
simple_title:         "Perhetien pituuden löytäminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi: Merkkijonon pituuden löytäminen Ruby-ohjelmoinnin avulla

Merkkijonon pituuden löytäminen voi tuntua yksinkertaiselta, mutta se on tärkeä osa ohjelmointia ja voi auttaa ratkaisemaan monia ongelmia. Tässä blogikirjoituksessa kerromme, miten löydät merkkijonon pituuden käyttäen Ruby-ohjelmointikieltä.

## Miten tehdä se

```Ruby
string = "Tervetuloa Ruby-maailmaan!"
puts string.length
```

Tulostus:

```
27
```

Yllä olevassa esimerkissä luomme muuttujan nimeltä "string", joka sisältää haluamamme merkkijonon. Sitten käytämme "length" -metodia tulostamaan merkkijonon pituuden.

Voit myös käyttää "count" -metodia löytääksesi tietyn merkin määrän merkkijonossa. Esimerkiksi:

```Ruby
string = "Tervetuloa Ruby-maailmaan!"
puts string.count("a")
```

Tulostus:

```
3
```

## Syvempi sukellus

Merkkijonon pituuden löytämisen lisäksi voit käyttää Ruby-ohjelmointia muokkaamaan ja manipuloimaan merkkijonoja. Voit esimerkiksi käyttää "reverse" -metodia kääntämään merkkijonon ympäri, tai "capitalize" -metodia muuttamaan merkkijonon ensimmäisen kirjaimen isoksi. Voit myös yhdistää kaksi merkkijonoa yhteen käyttämällä "+" -merkkiä.

## Katso myös

- [Ruby-ohjelmointikielen virallinen sivusto](https://www.ruby-lang.org/fi/)
- [Merkkijonojen muokkaaminen Rubyssa](https://www.rubyguides.com/2019/02/ruby-string-methods/)