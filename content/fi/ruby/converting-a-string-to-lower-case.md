---
title:                "Ruby: Merkkijonon muuntaminen pienaakkosiksi"
simple_title:         "Merkkijonon muuntaminen pienaakkosiksi"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi ihmiset haluavat muuttaa merkkijonon pieniksi kirjaimiksi? Tätä kysymystä voi kysyä monessa eri yhteydessä. Ehkä haluat vertailla kahta merkkijonoa, mutta case-sensitiivisyys haluaa tehdä sen hieman vaikeammaksi. Tai ehkä haluat vain varmistaa, että käyttäjän antama syöte on yhdenmukainen ja välttää mahdollisia virheitä. Joka tapauksessa, pienien kirjainten muuttaminen voi olla erittäin kätevä ja hyödyllinen toiminto.

## Miten

Tapoja muuttaa merkkijono pieniksi kirjaimiksi on monia, mutta tässä käytämme Ruby-koodia esimerkin avuksi. 

```Ruby
# Alkuperäinen merkkijono
string = "AUTTAKAA MINUA!"

# Muutetaan pieniksi kirjaimiksi
lowercase = string.downcase

# Tulostetaan muunnettu merkkijono
puts lowercase

# Output: auttakaa minua!
```

Tässä esimerkissä käytämme Ruby-metodia `downcase`, joka muuttaa kaikki merkit merkkijonossa pieniksi kirjaimiksi. Tämä on yksinkertainen tapa muuttaa merkkijono toivottuun muotoon.

## Syvemmälle

On hyvä tietää, että pieniksi kirjaimiksi muuttaminen toimii vain aakkosilla, joten numerot ja erikoismerkit pysyvät ennallaan. Tämä saattaa aiheuttaa ongelmia, jos tarvitaan täydellinen merkkien muunnos. Onneksi Rubyssa on myös `swapcase`-metodi, joka muuttaa merkkijonossa olevat isot kirjaimet pieniksi ja päinvastoin. Esimerkiksi:

```Ruby
string = "HeLlO WoRLd!"
swapped = string.swapcase
puts swapped
# Output: hElLo wOrlD!
```

Lisäksi Rubylla on myös monia muita hyödyllisiä tapoja työskennellä merkkijonojen kanssa, kuten `capitalize`, `chomp`, `strip`, jne. Kannattaa tutkia näitä metodeja ja löytää itselleen sopivin tapa käsitellä merkkijonoja.

## Katso myös

Tässä artikkelissa kävimme läpi yhden tavan muuttaa merkkijonon pieniksi kirjaimiksi Rubyssa. Jos haluat oppia lisää Ruby-ohjelmoinnista, suosittelen tutustumaan seuraaviin resursseihin:

- [Ruby oppimateriaalit](https://ruby-lang.org/en/documentation/): löydät täältä kaiken tarvittavan Ruby-ohjelmointiin liittyen.
- [RubyTapas](https://rubytapas.com/): Erinomainen sivusto, josta voit oppia lisää Ruby-kielen saloista.
- [Ruby-kirjat](https://www.goodreads.com/shelf/show/ruby): Täällä voit löytää listan suosituimmista Ruby-kirjoista, joista voi olla hyötyä opiskellessasi Rubya.

Toivottavasti tämä artikkeli auttoi sinua ymmärtämään paremmin merkkijonon muuttamisen alempiksi kirjaimiksi Rubyssa. Muista harjoitella ja löytää itsellesi sopivin tapa käsitellä merkkijonoja. Onnea matkaan!