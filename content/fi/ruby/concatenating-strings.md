---
title:                "Ruby: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi meidän tulisi käyttää merkkijonojen yhdistämistä Rubyssa? Merkkijonojen yhdistäminen on tärkeä osa ohjelmointia, koska se mahdollistaa eri tietojen yhdistämisen ja luomisen yhdeksi kokonaisuudeksi. Tämä voi olla hyödyllistä esimerkiksi silloin, kun haluamme luoda dynaamisia tekstejä tai tietokantojen kyselyjä.

## Kuinka

Merkkijonojen yhdistämiseen on monta tapaa, mutta yksi yleisimmistä on käyttää `+` -operaattoria. Tämä yhdistää kaksi merkkijonoa yhdeksi kokonaisuudeksi. Katso alla olevaa esimerkkiä:

```Ruby
first_name = "Matti"
last_name = "Meikäläinen"
full_name = first_name + last_name
puts full_name
```

Tämä koodi tulostaa "MattiMeikäläinen". Huomaatko, että välilyöntiä ei ole? Tämä johtuu siitä, että `+` -operaattori vain yhdistää kaksi merkkijonoa ilman ylimääräisiä välilyöntejä.

Toinen yleinen tapa yhdistää merkkijonoja on käyttää `concat()` -metodia. Tämä metodi yhdistää merkkijonot ja lisää tarvittaessa välilyönnin alkuun. Alla olevassa esimerkissä käytämme myös `gsub()` -metodia korvaamaan `å` -merkin `aa` -merkillä.

```Ruby
first_name = "Matti"
last_name = "Mäkinen"
full_name = first_name.concat(" ", last_name)
full_name = full_name.gsub("å", "aa")
puts full_name
```

Tulostus olisi nyt "Matti Mäkinen".

## Syvemmälle

On tärkeää muistaa, että merkkijonojen yhdistäminen voi vaikuttaa ohjelman suoritusnopeuteen. Jos yhdistät suuria määriä merkkijonoja, voi olla parempi käyttää `<<` -operaattoria tai `push()` -metodia. Nämä eivät luo uutta merkkijonoa, vaan muokkaavat olemassa olevaa.

Voit myös käyttää `.join()` -metodia, joka yhdistää taulukon merkkijonoiksi käyttäen annettua erotinta. Tämä voi olla hyödyllistä, kun haluat yhdistää useita merkkijonoja yhteen ilman `+` -operaattoria. Alla olevassa esimerkissä käytämme tätä metodia yhdistämään taulukon merkkijonoiksi ja tulostamme ne konsoliin.

```Ruby
names = ["Matti", "Mäkinen", "Meikäläinen"]
full_name = names.join(" ")
puts full_name
```

Tämä tulostaisi "Matti Mäkinen Meikäläinen".

## Katso myös

- Ruby Docs: https://ruby-doc.org/core-2.6/String.html#method-i-2B
- Concat, push ja join: https://medium.com/@AndrewSchechterman/ruby-join-concat-push-and-just-do-it-the-basics-ac5b75611aff
- Strings in Ruby: https://www.rubyguides.com/2019/08/ruby-strings/