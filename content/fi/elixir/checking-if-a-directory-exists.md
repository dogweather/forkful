---
title:    "Elixir: Tarkistetaan löytyykö hakemistoa"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Miksi

Miksi sinun kannattaa tarkistaa, onko hakemisto olemassa? Hakemistojen tarkistaminen voi olla tärkeää sovelluksille, jotka tarvitsevat pääsyn tiettyihin tiedostoihin tai resursseihin. Tämä varmistaa, että sovelluksesi toimii sujuvasti ja virheettömästi.

## Miten

Tarkastaaksesi, onko hakemisto olemassa Elixirillä, voit käyttää `File.dir?`-funktiota. Tämä funktio ottaa argumenttina hakemiston polun ja palauttaa `true`, jos hakemisto on olemassa, ja `false`, jos sitä ei ole.

```Elixir
File.dir?("polku/hakemistoon")
# => true
```

Voit myös käyttää `File.cwd/0`-funktiota saadaksesi nykyisen työhakemiston polun ja tarkistaa sen avulla, onko hakemisto olemassa.

```Elixir
työhakemisto = File.cwd()
# => "käyttäjä/kansio/työhakemisto"

File.dir?(työhakemisto)
# => true
```

Usein haluamme myös tarkistaa, onko tiedostopolku olemassa ennen hakemiston tarkistamista. Tällöin voimme käyttää `File.exists?`-funktiota, joka palauttaa `true`, jos tiedostopolku on olemassa, ja `false`, jos se ei ole.

```Elixir
File.exists?("polku/olemattomaan/hakemistoon")
# => false

File.exists?(työhakemisto)
# => true
```

## Syvemmälle

Elixirissa tapahtumienkäsittelijät tarjoavat myös vaihtoehtoisen tavan tarkistaa hakemistoja. Voit käyttää `Dir`-moduulia ja sen `exists?/1`-funktiota tarkistamaan, onko hakemisto olemassa.

```Elixir
Dir.exists?("polku/hakemistoon")
# => true
```

Tämä toiminto käyttää taustalla `File.dir?`-funktiota ja palauttaa myös `true` tai `false`.

Voit myös käyttää `File.stat/1`-funktiota saadaksesi lisätietoja tiedostosta tai hakemistosta. Tämä palauttaa `Stats`-rakenteen, joka sisältää muun muassa tiedoston tai hakemiston luomisajan ja viimeisen muokkausajan.

```Elixir
File.stat("polku/hakemistoon")
# => {:ok, %File.Stat{atime: ..., ctime: ..., birthtime: ..., ... }}
```

Voit löytää lisätietoja Elixirin tiedostohoitoon liittyvistä toiminnoista [virallisesta dokumentaatiosta](https://hexdocs.pm/elixir/File.html) ja [Dir-moduulin dokumentaatiosta](https://hexdocs.pm/elixir/Dir.html).

## Katso myös

- [Elixirin virallinen dokumentaatio](https://hexdocs.pm/elixir/)
- [Elixirin "Getting Started" -opas](https://elixir-lang.org/getting-started/introduction.html)
- [Elixirin kansainvälinen yhteisö](https://elixir-lang.org/community.html)