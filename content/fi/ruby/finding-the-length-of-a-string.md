---
title:    "Ruby: Merkkijonon pituuden löytäminen"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Miksi

On olemassa monia tilanteita, joissa tarvitsemme tietää, kuinka monta merkkiä tai kirjainta on tietyssä merkkijonossa. Tämä taito on erittäin hyödyllinen esimerkiksi tekstinmuokkausohjelmissa tai laskurin luomisessa. Tässä Ruby-ohjelmoinnin blogimerkinnässä opimme kuinka löytää merkkijonon pituus.

## Kuinka

Koodiesimerkkimme käyttävät Rubyn `length`-metodia, joka palauttaa merkkijonon pituuden. Huomaa, että metodi toimii myös muiden tietotyyppien, kuten taulukoiden ja hashien, kanssa.

```Ruby
# Luodaan muuttuja nimeltä "merkkijono" johon tallennetaan tietyt merkit
merkkijono = "Hei, maailma!"

# Käyttämällä "length"-metodia voimme selvittää merkkijonon pituuden
puts "Merkkijonon pituus on #{merkkijono.length}" #Output: 13
```

Kuten näemme, metodi on helppokäyttöinen ja palauttaa halutun tiedon suoraan. Voimme myös tallentaa pituuden uuteen muuttujaan ja käyttää sitä muilla tavoilla.

## Syväsukellus

Entä jos haluamme laskea vain tietyt merkit tietyillä väleillä? Tähän tarkoitukseen voimme käyttää `count`-metodia, joka ottaa parametreikseen halutun merkin tai merkkijonon ja laskee niiden määrän annetusta merkkijonosta.

```Ruby
# Lasketaan montako "a"-kirjainta on merkkijonossa
merkkijono = "Tämä on testi"
puts "Merkkijonossa on #{merkkijono.count("a")} a-kirjainta" #Output: 1
```

Tässä esimerkissä halusimme laskea vain "a"-kirjainten määrän, mutta voimme käyttää `count`-metodia myös monimutkaisempiin laskuihin esimerkiksi regexin avulla.

## Katso myös

- [Rubyn dokumentaatio merkkijonon pituudesta](https://ruby-doc.org/core-3.0.0/String.html#method-i-length)
- [Codecademyn tutoriaali merkkijonojen käsittelystä Rubylla](https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-string-basics/cheatsheet)