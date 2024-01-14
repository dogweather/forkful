---
title:                "Elixir: Väliaikaistiedoston luominen"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Jokainen meistä on varmasti joutunut jossain vaiheessa luomaan väliaikaisen tiedoston, joka tarvitaan vain hetken aikaa. Tämä on yleinen tarve, esimerkiksi tiedon tallentamiseen, kunnes se siirretään pysyvään tallennuspaikkaan tai kun luodaan väliaikainen varmuuskopio. Tiedoston luominen vain hetkeksi voi tuntua turhalta, mutta se voi olla hyödyllistä monissa tilanteissa. Elixirillä voit helposti luoda väliaikaisen tiedoston ja tässä blogikirjoituksessa opit, miten se tehdään.

## Miten

Elixirillä voit luoda väliaikaisen tiedoston helposti käyttämällä `File` moduulia. Voit käyttää `File.open!/2` funktiota ja antaa parametreiksi tiedoston nimen ja halutun toiminnon.

```Elixir
File.open!("temp.txt", [:write])
```

Tämä luo uuden tiedoston nimeltä `temp.txt`, johon voit kirjoittaa haluamasi tiedot. Voit myös käyttää muita toimintoja, kuten `:read` tai `:append`, tarpeidesi mukaan.

Voit myös määrittää haluamasi hakemiston, johon tiedosto luodaan, antamalla `path` parametrin funktiolle.

```Elixir
File.open!("temp.txt", [:write], path: "/kansio")
```

Kun luotu tiedosto ei ole enää tarpeellinen, voit käyttää `File.rm/1` funktiota poistaaksesi sen.

```Elixir
File.rm("temp.txt")
```

Jos haluat luoda väliaikaisen tiedoston, johon voit tallentaa dataa ja johon vain tietyt prosessit voivat kirjoittaa, voit käyttää `Tempfile` moduulia. Tämä luo turvallisen tiedoston, johon ainoastaan siihen liitetty prosessi voi kirjoittaa.

```Elixir
file = Tempfile.open("temp.txt")
file.write("Tämä on väliaikainen tiedosto.")
file.close
```

## Syvällinen sukellus

File moduulin lisäksi Elixirillä on myös `Path` ja `Path.wildcard/2` funktiot, joiden avulla voit luoda väliaikaisia tiedosto- ja hakemistopolkuja.

Jos haluat tiedon tallentamiseen käyttää jotain muuta kuin tiedostoa, voit käyttää `GenServer` moduulia. Tämä antaa sinulle mahdollisuuden luoda prosessi, joka tallentaa dataa muistiin väliaikaisesti ja poistuu, kun se ei enää ole tarpeellinen.

## Katso myös

- [Elixirin virallinen dokumentaatio](https://hexdocs.pm/elixir/File.html)
- [Elixir School -opas tiedostonhallintaan](https://elixirschool.com/en/lessons/basics/file/)
- [Tempfile moduulin dokumentaatio](https://hexdocs.pm/elixir/Tempfile.html)