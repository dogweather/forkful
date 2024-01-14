---
title:    "Elixir: Lukeminen tekstitiedostosta"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Miksi lukea tekstitiedostoja Elixirilla?

Elixir on toimiva ja monipuolinen ohjelmointikieli, joka tarjoaa laajan valikoiman työkaluja datan käsittelyyn. Yksi näistä työkaluista on tiedostojen lukeminen, mikä voi olla hyödyllistä esimerkiksi datan analysoinnissa tai käsittelyssä. Tässä artikkelissa näytämme, miten voit lukea tekstitiedostoja Elixirilla.

## Miten?

Tiedostojen lukeminen Elixirilla on helppoa ja suoraviivaista. Voit tehdä sen yksinkertaisesti käyttämällä File-moduulia ja sen lukufunktioita. Esimerkiksi voit lukea tekstirivit yksitellen ja tulostaa ne konsoliin seuraavasti:

```elixir
file = File.open("tiedosto.txt")
Enum.each(File.stream!(file), fn line -> IO.puts(line) end)
File.close(file)
```

Yllä oleva koodi avaa tiedoston "tiedosto.txt", lukee sen sisällön riveittäin ja tulostaa jokaisen rivin konsoliin. Huomaathan, että tiedoston lukeminen tapahtuu streamin kautta, mikä mahdollistaa suuren datamäärän käsittelyn tehokkaasti.

Voit myös lukea ja käsitellä tiedoston sisältöä tavallisena listana käyttämällä File.read!/1 -funktiota. Alla oleva koodi esimerkiksi laskee, kuinka monta sanaa tiedostossa on.

```elixir
file = File.read!("tiedosto.txt")
words = Enum.reduce(String.split(file), 0, fn word, acc -> acc + 1 end)
IO.puts("Tiedostossa on #{words} sanaa.")
```

## Syvempi sukellus

Tiedostojen lukeminen Elixirilla on mahdollista myös muilla tavoilla, kuten esimerkiksi erilaisten tietorakenteiden avulla. Voit esimerkiksi luoda listan tiedoston sisällöstä käyttämällä Enum.into/3 -funktiota.

```elixir
file = File.read!("tiedosto.txt")
lines = file |> String.split("\n")
Enum.into(lines, [])
```

Tässä tapauksessa lista nimeltä "lines" sisältää tiedoston rivit ja voit käsitellä niitä haluamallasi tavalla. Voit myös käyttää Map-moduulia, jolla voit luoda hajautetun tietorakenteen tiedoston avaimista ja arvoista.

Lisätietoa tiedostojen lukemisesta Elixirilla löydät esimerkiksi [virallisesta dokumentaatiosta](https://hexdocs.pm/elixir/File.html) ja [tästä Honeypot-blogikirjoituksesta](https://www.honeypot.io/blog/elixir-file-i-o-tutorial/).

# Katso myös

- [Elixir-opas: Filen käyttö tiedostojen lukemiseen ja kirjoittamiseen](https://elixir-lang.org/getting-started/File.html)
- [Elixir School: File-moduulin käyttö tiedostojen käsittelyssä](https://elixirschool.com/lessons/basics/file-io/)
- [Honeypot-blogikirjoitus: Tiedostojen käsittely Elixirilla](https://www.honeypot.io/blog/elixir-file-i-o-tutorial/)