---
title:                "Elixir: S merkkijonon suurenna"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisimme muuttaa merkkijonon isoiksi kirjaimiksi? On monia syitä, joista yksi saattaa olla koodauksen yhtenäistäminen. Jos haluamme varmistaa, että kaikki käyttäjät syöttävät esimerkiksi salasanat samassa muodossa, voimme käyttää koodilogiikkaa muuttamaan käyttäjän antaman merkkijonon isoiksi kirjaimiksi. Tämä auttaa myös välttämään mahdollisia virheitä ja yhdenmukaistamaan käyttäjätietoja.

## Miten

Elixirissä merkkijonon muuttaminen isoiksi kirjaimiksi on hyvin yksinkertaista käyttämällä moduulia `String` ja sen funktiota `upcase`. Käyttämällä tätä funktiota, voimme muuttaa merkkijonon kaikki kirjaimet isoiksi `String.upcase("merkkijono")` ja saamme tulokseksi "MERKKIJONO". Alla on esimerkki koodista ja sen tulosteen voit nähdä alla olevassa lausekkeessa.

```Elixir
stringi = "terve"
String.upcase(stringi)
```

Tulostaa: "TERVE"

## Syvällinen sukellus

Vaikka merkkijonon muuttaminen isoiksi kirjaimiksi on helppoa Elixirissä, on tärkeää tietää tarkkaan miten tämä prosessi toimii. `String.upcase`-funktio käyttää Unicode-standardia muuttaessaan merkkijonon kirjainten kuvakkeet isoiksi kirjaimiksi. Tämä tarkoittaa, että jos merkkijonossa on esimerkiksi ääkkösiä, ne muutetaan myös isoiksi kirjaimiksi oikein. On myös hyvä huomata, että `String.upcase` ei muuta merkkijonon sisältöä, vaan palauttaa uuden merkkijonon. Jos haluamme muuttaa alkuperäisen merkkijonon, voimme käyttää `String.upcase!`-funktiota.

## Katso myös

- [Elixir String -moduulin dokumentaatio](https://hexdocs.pm/elixir/String.html)
- [Tietoa Unicode-standardista](https://unicode.org/versions/latest/)