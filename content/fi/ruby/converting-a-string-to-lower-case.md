---
title:                "Ruby: Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Miksi

 Monet Ruby-ohjelmoijat saattavat joutua muuttamaan merkkijonoja pieniksi kirjaimiksi, jotta ne vastaavat kirjastoista tai tietokannasta saatavia tietoja. Tämä voi auttaa välttämään haittatilanteita tai virheitä tietojen käsittelyssä.

## Miten tehdä

Merkkijonon muuttaminen pieniksi kirjaimiksi on helppoa Rubyssa. Tätä varten voit käyttää `downcase`-metodia:

```Ruby
string = "Moi maailma!"
puts string.downcase
```

Tulostaa:

```Ruby
moi maailma!
```

Voit myös käyttää `downcase!` metodia, joka muuttaa alkuperäisen merkkijonon sisältöä:

```Ruby
string = "Moi maailma!"
string.downcase!
puts string
```

Tulostaa:

```Ruby
moi maailma!
```

## Syvällinen sukellus

Rubyssa on myös muita tapoja muuntaa merkkijonoja pieniksi kirjaimiksi, kuten `swapcase`-metodi, joka muuttaa merkkijonon isot kirjaimet pieniksi ja päinvastoin. Voit myös käyttää `capitalize`-metodia, joka muuttaa merkkijonon ensimmäisen kirjaimen isoksi.

Merkkijonojen muuntaminen pieniksi kirjaimiksi on tärkeä osa tietojen käsittelyä Rubyssa. Se auttaa varmistamaan, että tiedot ovat yhteensopivia ja virheetön.

# Katso myös

- [Ruby String Documentation](https://ruby-doc.org/core-2.7.2/String.html)
- [Codecademy: Manipulating Strings in Ruby](https://www.codecademy.com/courses/learn-ruby/lessons/strings-in-ruby/exercises/manipulating-strings-in-ruby)