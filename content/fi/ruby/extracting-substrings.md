---
title:                "Ruby: Alaohjelman nimet"
simple_title:         "Alaohjelman nimet"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi: 

Miksi joku haluaisi osallistua alamerkkijonojen erotteluun Ruby-ohjelmoinnilla? 

On monia syitä, miksi tämä voi olla hyödyllistä ohjelmoijalle. Yksi yleinen syy voisi olla, että käsiteltävässä merkkijonossa on tiettyä tietoa, jota tarvitaan erikseen. Esimerkiksi, jos ohjelmassa on annettu henkilötunnus, saattaa olla hyödyllistä saada erillinen alamerkkijono syntymäajan osista. Tai, jos käyttäjä syötti postinumeron, alamerkkijonolla voisi olla hyödyllinen tieto siitä, mihin kaupunkiin se liittyy.

## Kuinka:

Seuraavassa on esimerkkejä siitä, kuinka voit käyttää Rubya erottamaan alamerkkijonoja. 

```Ruby
# Alustetaan merkkijono
merkkijono = "Tämä on esimerkkilause."

# Tulostaa viisi ensimmäistä merkkiä
puts merkkijono[0, 5]
# => Tämä

# Tulostaa viisi viimeistä merkkiä
puts merkkijono[-5, 5]
# => lause

# Tulostaa merkkijonon kolmannesta merkistä alkaen viisi merkkiä
puts merkkijono[2, 5]
# => m on

# Käyttäen regexp-merkintä kiinteän merkkimäärän sijaan

# Tulostaa kaikki isolla kirjaimella alkavat sanat
puts merkkijono.scan(/\b[A-Z]\w*/)
# => Tämä

# Tulostaa kaikki numerot
puts merkkijono.scan(/\d+/)
# => tyhjä taulukko, sillä merkkijonossa ei ole numeroita
```

## Syvällinen tarkastelu:

Ruby tarjoaa monia erilaisia tapoja erottaa alamerkkijonoja. Yllä olevissa esimerkeissä käytämme `[]`-operaattoria ja `scan`-metodia. `[]`-operaattori ottaa vastaan aloitusindeksin ja halutun merkkimäärän, kun taas `scan`-metodi käyttää regexp-merkintää löytääkseen halutun merkkimäärän. 

On myös muita vaihtoehtoja, kuten `slice`-metodi ja `match`-metodi. Lisäksi on mahdollista käyttää regexp-merkintää `partition`-metodilla saadakseen alueen ja erottelemaan sen kolmeen eri osaan.

Erotaessa alamerkkijonoja on tärkeää olla tarkka siitä, minkä merkkimäärän haluat ja missä kohdassa merkkijonoa se sijaitsee. Joissakin tapauksissa, kuten ha