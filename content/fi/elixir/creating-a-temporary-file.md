---
title:                "Elixir: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Miksi luoda väliaikainen tiedosto Elixir-ohjelmoinnissa?

Väliaikaisten tiedostojen luominen voi olla hyödyllistä, kun halutaan tallentaa väliaikaisia tietoja, jotka eivät ole välttämättömiä ohjelman toiminnalle. Esimerkiksi väliaikaiseen tiedostoon voi tallentaa väliaikaisesti laskennallisia tuloksia tai väliaikaisesti käsiteltäviä tietoja, jotka poistetaan käytön jälkeen. Elixir-ohjelmoinnissa väliaikaisten tiedostojen luominen on helppoa ja kätevää.

## Miten luoda väliaikainen tiedosto Elixir-ohjelmoinnissa?

Väliaikaisen tiedoston luominen Elixir-ohjelmoinnissa tapahtuu `:tempfile`-moduulin avulla. Tämä moduuli tarjoaa toiminnallisuuden luoda väliaikainen tiedosto ja palauttaa sen polun. Tiedostolle voi myös asettaa halutun nimen, ja sen voi luoda halutussa hakemistossa.

```Elixir
{:ok, path} = :tempfile.open("prefix", "tmp")
```

Tässä esimerkissä luodaan väliaikainen tiedosto käyttämällä `:tempfile`-moduulin `open`-funktiota, ja annetaan tiedostolle nimeksi "prefix" ja hakemistoksi "tmp". Tämän jälkeen tiedoston polku tallennetaan `path`-muuttujaan.

Väliaikaisen tiedoston voi myös poistaa käytön jälkeen `:file`-moduulin `delete`-funktion avulla.

```Elixir
:file.delete(path)
```

Kokonaisia tiedostoja voi myös lukea ja kirjoittaa käyttäen `:file`-moduulin funktioita `read` ja `write`.

```Elixir
{:ok, file} = File.open(path, [:write]) # Avataan tiedosto kirjoitusta varten
IO.write(file, "Hello World!") # Kirjoitetaan tiedostoon
File.close(file) # Suljetaan tiedosto

{:ok, file} = File.open(path, [:read]) # Avataan tiedosto lukemista varten
IO.puts(IO.read(file)) # Tulostetaan tiedoston sisältö
File.close(file) # Suljetaan tiedosto
```

## Syvällisempi sukellus väliaikaisten tiedostojen luomiseen Elixir-ohjelmoinnissa

Väliaikaisten tiedostojen luominen Elixir-ohjelmoinnissa tapahtuu käyttäen `:tempfile`-moduulia ja sen `open`-funktiota. Funktion avulla voidaan määrittää tiedoston nimi, hakemisto sekä muut parametrit, kuten tiedoston luomisen tapa (esimerkiksi aikaleiman perusteella).

Väliaikainen tiedosto luodaan käyttöjärjestelmän väliaikaiseen hakemistoon, mikäli hakemistoa ei ole erikseen määritetty. Tiedoston nimeen voi myös lisätä etuliitteen ja/tai jälkiliitteen `:tempfile`-moduulin avulla.

Väliaikaisen tiedoston luominen voi myös vaatia käyttöoikeuksia. Tämän takia Elixir-ohjelmoijan tulee huomioida, että ohjelmalla on tarvittavat oikeudet tiedoston luomiseen ja poistamiseen.

# Katso myös

- `:tempfile`-moduulin dokumentaatio: https://hexdocs.pm/elixir/Tempfile.html
- `:file`-moduulin dokumentaatio: https://hexdocs.pm/elixir/File.html