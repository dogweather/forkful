---
title:    "Gleam: Muuntaa merkkijono pieniksi kirjaimiksi"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Miksi

Miksi haluaisit muuntaa merkkijonon pieniksi kirjaimiksi? On monia syitä, miksi tämä voi olla hyödyllistä ohjelmoinnissa, kuten helpottaa tietojen vertailua tai varmistaa yhdenmukaisuus tietokantahakuissa.

Merkkijonon muuttaminen pieniksi kirjaimiksi voi myös olla hyödyllistä luoda käyttäjäystävällisiä ohjelmia, sillä käyttäjät eivät välttämättä aina kirjoita merkkijonoja oikeassa muodossa.

# Kuinka

```Gleam
String.to_lower("HELLO") #=> "hello"
String.to_lower("I LoVe Gleam") #=> "i love gleam"
```

Voit käyttää String.to_lower-funktiota muuntaaksesi merkkijonon pieniksi kirjaimiksi. Tässä funktiossa käytetään Unicode-normalisointia varmistaakseen, että merkkijonon erikoismerkit käsitellään oikein.

```Gleam
String.to_lower("Käyttäjänimi") #=> "käyttäjänimi"
```

Gleamin mukaan tämä funktio palauttaa aina uuden merkkijonon, joten alkuperäinen merkkijono säilyy muuttumattomana.

# Syvällinen sukellus

Merkkijonon muuntaminen pieniksi kirjaimiksi voi olla tärkeää myös ohjelmistojen yhteensopivuuden ja tietoturvan kannalta.

Unicode-standardi määrittelee, että jokainen merkki voi esiintyä useissa eri muodoissa, esimerkiksi pienaakkosena tai isoakkosena. Tämä voi aiheuttaa ongelmia vertaillessa merkkijonoja, mikäli ne eivät ole samassa muodossa. Siksi on tärkeää käyttää Unicode-normalisointia merkkijonojen muuntamiseksi yhtenäiseen muotoon.

Merkkijonon muuntaminen pieniksi kirjaimiksi voi myös auttaa torjumaan tietoturvariskejä, kuten SQL-injektioita. Pienille ja isoille kirjaimille herkkien tietokantojen kanssa työskennellessä tietokantahaut voidaan muuntaa samassa muodossa oleviin merkkijonoihin, mikä voi estää haitallisten kyselyjen lähettämisen.

# Katso myös

- Gleamin dokumentaatio: https://gleam.run/documentation/std-lib/String.html#to_lower/1
- Unicode-standardi: https://unicode.org/
- SQL-injektiot: https://owasp.org/www-community/attacks/SQL_Injection