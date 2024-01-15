---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "Elixir: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittamalla tekstilahetis voidaan tallentaa tietoa pysyvästi ja jakaa sitä muille ohjelmille ja käyttäjille.

## Kuinka

```elixir
# Avataan uusi tiedosto
file = File.open("uusi_tiedosto.txt", [:write])

# Kirjoitetaan tiedostoon
File.write(file, "Tämä on tekstiä uudessa tiedostossa")
File.close(file)

# Luetaan tiedosto
File.read("uusi_tiedosto.txt")
```

Tulostus: "Tämä on tekstiä uudessa tiedostossa"

## Syvällinen syventymis

Tekstilahde on keskeinen osa Elixir-ohjelmointikieltä ja se on integroitu vahvasti kieleen. Textiä voidaan käyttää eri tapoja, kuten tallentaa tietoa, luoda lokitiedostoja tai jopa luoda käyttöliittymään sisältöä. Jatka tutustumista tähän tärkeään osa-alueeseen ja löydä uusia tapoja käyttää tekstilahetis Elixirin kanssa.

## Katso myös
- [Elixirin virallinen dokumentaatio tekstilahetiksi](https://hexdocs.pm/elixir/File.html)
- [Elixirin tekstilahetin perusteet](https://www.tutorialspoint.com/elixir/elixir_files.htm)
- [Käyttötapoja Elixirin tekstilahetille](https://www.dailydrip.com/blog/working-with-elixir-binary-text-files)