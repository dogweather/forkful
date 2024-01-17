---
title:                "Tilapäistiedoston luominen"
html_title:           "Elixir: Tilapäistiedoston luominen"
simple_title:         "Tilapäistiedoston luominen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# What & Why?

Luonnollisesti ohjelmointitehtävät kuluttavat tietokoneen resursseja, ja siksi on tärkeää varmistaa, että kaikki käytetyt tiedostot vapautetaan oikea-aikaisesti. Väliaikaisten tiedostojen luominen on yksi tapa hallita resursseja ja varmistaa, että oleelliset tiedostot eivät jää turhaan järjestelmään.

# How to:

Elixirin avulla voimme luoda väliaikaisen tiedoston ```Tempfile``` -moduulin avulla. Se tarjoaa meille yksinkertaisen tavan luoda ja poistaa väliaikaisia tiedostoja ohjelmassamme. Katso esimerkki alla:

```Elixir
tempfile = Tempfile.new("nimi")
```

Kun tiedosto on luotu, voimme kirjoittaa siihen haluamamme tiedot ja käyttää sitä ohjelmassamme niin kauan kuin tarvitsemme. Kun olemme valmiita, voimme poistaa tiedoston seuraavasti:

```Elixir
File.delete!(tempfile.path)
```

Tämä varmistaa, että tiedosto poistetaan varmasti ja säästää resursseja, joita ohjelma tarvitsee.

# Deep Dive:

Tämä menetelmä on ollut käytössä jo pitkään ja on hyvä tapa hallita resursseja. Toinen vaihtoehto väliaikaisten tiedostojen luomiseen on käyttää Unix järjestelmille tarkoitettua ```mktemp``` -komentoa. Se toimii samalla tavalla, mutta vaatii hieman enemmän koodia. Elixirin ```Tempfile``` -moduuli tekee tämän kaiken meille yksinkertaisemmaksi.

Moduulin sisäisesti se luo lisäksi ```File.open/2``` -funktion avulla väliaikaisen tiedoston ja palauttaa siitä avoimen tiedoston kahvana käytettäväksi ohjelmassa. Tämä tarjoaa lisää varmuutta tiedoston avauksessa ja käytössä.

# See Also:

Voit lukea lisää Elixirin ```Tempfile``` -moduulista täältä: https://hexdocs.pm/elixir/Tempfile.html.