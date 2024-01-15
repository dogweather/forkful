---
title:                "Tekstitiedoston lukeminen"
html_title:           "Elixir: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointiprojekteissa on tarve lukea tekstitiedostoja. Tämä voi olla esimerkiksi datan käsittelyä tai tiedostojen kopiointia varten. Elixirin avulla tämä on helppoa ja tehokasta.

## Miten

Lueessa tekstitiedostoja Elixirillä, käytetään File moduulia. Ensiksi, avataan haluttu tiedosto `File.open()` funktion avulla. Tämän jälkeen voidaan käyttää `IO.stream()` funktiota luomaan datavirta tiedostolle. Lopuksi tiedosto suljetaan `File.close()` avulla.

```Elixir
file = File.open("tiedosto.txt", [:read])    # Avaa tiedosto
stream = IO.stream(file)                    # Luo datavirta
IO.read(stream, :all)                       # Tulostaa koko tekstin
File.close(file)                            # Sulje tiedosto
```

Output: `"Tämä on esimerkki tekstiä."`

## Syvemmälle

Reading files in Elixir is actually done synchronously, meaning that the entire file is read into memory at once. This may not be ideal for very large files, as it could potentially cause memory issues. To overcome this, Elixir also offers the option to read files asynchronously using `File.read()` and `Stream.resource()`.

## Katso myös

- [File moduuli](https://hexdocs.pm/elixir/File.html)
- [IO moduuli](https://hexdocs.pm/elixir/IO.html)
- [Stream moduuli](https://hexdocs.pm/elixir/Stream.html)