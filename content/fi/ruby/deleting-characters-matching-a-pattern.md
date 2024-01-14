---
title:                "Ruby: Kaavan mukaisesti vastaavien merkkien poistaminen"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä miksi saattaisimme haluta poistaa merkkejä, jotka vastaavat tiettyä mallia ohjelmointikielestä. Yksi yleinen syy on, kun haluamme puhdistaa tai muokata esimerkiksi tekstiä tai tietokantoja ennen niiden käyttöä.

## Miten

Käyttämällä ruby-kirjaston String.replace -metodin voimme helposti poistaa merkkejä, jotka vastaavat haluttua määritettyä mallia käyttäen regex- tai oliopohjaista lähestymistapaa.

```Ruby
text = "Tervetuloa, tämä on esimerkkiteksti 12345"
clean_text = text.gsub(/[0-9]/, "") # poistaa kaikki numerot tekstimuodosta
puts clean_text
# Tulostaa "Tervetuloa, tämä on esimerkkiteksti "
```

## Syvempi sukellus

Ruby:ssa on monia hyödyllisiä metodeja merkkijonojen muokkaamiseen, kuten esimerkiksi gsub!, joka muokkaa merkkijonoa suoraan ilman väliaikaista muuttujaa. Myös regex-säännöt voivat olla hyödyllisiä, jos haluamme muokata vain tiettyjä merkkejä tai merkkijonoja.

## Katso myös

- [Ruby String -dokumentaatio](https://ruby-doc.org/core-2.6.3/String.html)
- [Regex -ohjeet](https://www.regular-expressions.info/)
- [Ruby regex esimerkkejä](https://www.rubyguides.com/2015/06/ruby-regex/)