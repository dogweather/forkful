---
title:    "Elixir: Muunna merkkijono pienaakkosiksi"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Miksi

Joskus Elixir-ohjelmoinnissa on tarpeen muuttaa merkkijono pieniksi kirjaimiksi. Tämä voi johtua esimerkiksi tarpeesta vertailla merkkijonoja tai luoda yhtenäinen formaatti.

## Miten

Voit muuttaa merkkijonon pieniksi kirjaimiksi käyttämällä funktiota `String.downcase/1`. Se ottaa parametrina merkkijonon ja palauttaa uuden merkkijonon, jossa kaikki kirjaimet ovat pieniä.

```Elixir
String.downcase("ELIXIR") #=> "elixir"
String.downcase("Hello World") #=> "hello world"
```

## Syvähdyssukellus

On tärkeää huomata, että merkkijonon muuttaminen pieniksi kirjaimiksi ei muuta alkuperäistä merkkijonoa, vaan palauttaa aina uuden merkkijonon. Tämä on hyödyllistä, jos haluat säilyttää alkuperäisen merkkijonon sellaisenaan.

Voit myös muuttaa merkkijonon pieniksi kirjaimiksi käyttämällä funktiota `String.to_lower/1`. Tämä funktio tekee saman asian kuin `String.downcase/1`, mutta se hyväksyy vain yhden parametrin, joka voi olla mikä tahansa tyyppi, joka voidaan muuntaa merkkijonoksi.

```Elixir
String.to_lower(123.45) #=> "123.45"
String.to_lower(:ELIXIR) #=> "elixir"
```

## Katso myös

- [Elixirin dokumentaatio merkkijonoista](https://hexdocs.pm/elixir/String.html)
- [Stringin ja Atomien ero Elixirissa](https://medium.com/@davekiss/helpers-for-working-with-strings-and-atoms-in-elixir-4cedf98404c9)