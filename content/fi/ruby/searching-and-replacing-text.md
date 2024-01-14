---
title:    "Ruby: Hakeminen ja tekstin korvaaminen"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Miksi

Kirjoittaessa koodia on usein tarpeen muuttaa olemassa olevaa tekstiä. Tämä voi johtua esimerkiksi kirjoitusvirheistä tai tarpeesta päivittää tietyt tiedot projektille. Onneksi, Rubyssa on helppo käyttää tekstiä etsimään ja korvaamiseen tarvittavia toimintoja.

## Miten

Käytämme `gsub`-metodia tekstinpätkien etsimiseen ja korvaamiseen. Tämä metodi toimii seuraavasti:

```
Ruby
text = "Tämä on esimerkki tekstistä"
new_text = text.gsub("esimerkki", "uusi esimerkki")

puts new_text
```

Tämä tuottaa seuraavan tulosteen:

```
"Uusi esimerkki tekstistä"
```

Kuten huomaat, `gsub`-metodi korvasi ensimmäisen parametrin esiintymät tekstissä toisella parametrilla. Voimme myös antaa kolmannen parametrin, joka rajoittaa korvausten määrää:

```
Ruby
text = "1, 2, 3, 4, 5"
new_text = text.gsub(",", "-", 2)

puts new_text
```

Tulostus on seuraava:

```
"1-2-3, 4, 5"
```

Toinen hyödyllinen metodi on `gsub!`, joka muokkaa tekstiä suoraan sen sijaan, että palauttaisi uuden version. Esimerkiksi:

```
Ruby
text = "Tämä on esimerkki tekstistä"
text.gsub!("esimerkki", "uusi esimerkki")

puts text
```

Tulostus:

```
"Uusi esimerkki tekstistä"
```

## Syvempi sukellus

`gsub`-metodia voi käyttää myös säännöllisten lausekkeiden avulla. Tämä antaa mahdollisuuden etsiä ja korvata monimutkaisempia tekstinpätkiä. Esimerkiksi voimme korvata kaikki isot kirjaimet pienillä kirjaimilla seuraavalla tavalla:

```
Ruby
text = "Tämä on ESIMERKKI tekstistä"
new_text = text.gsub(/[A-Z]/, &:downcase)

puts new_text
```

Tulostus:

```
"Tämä on ESIMERKKI tekstistä"
```

## Katso myös

- [Ruby Dojo's Regular Expressions](https://ruby-dojo.com/regular-expressions-in-ruby/)
- [Ruby Documentation for String#gsub](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)