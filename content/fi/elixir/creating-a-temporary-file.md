---
title:    "Elixir: Väliaikaisen tiedoston luominen"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelman suorittamisen aikana on tarpeen tallentaa väliaikainen tiedosto, esimerkiksi väliaikaisen tiedon kanssa työskentelyyn tai muokattavan tiedoston varmuuskopiointiin. Tässä blogikirjoituksessa opit, miten voit luoda väliaikaisen tiedoston Elixir-ohjelmassa ja miksi se voi olla hyödyllistä.

## Miten

Useimmissa tilanteissa väliaikaisen tiedoston luominen sujuu helposti käyttämällä ```File.open_temp/2``` -funktiota Elixirissä. Alla on esimerkkejä, joissa luodaan väliaikainen tiedosto nimeltä "temp.txt" ja kirjoitetaan siihen merkkijono "Tämä on väliaikainen tiedosto".

```Elixir
{:ok, file} = File.open_temp("temp.txt")
IO.write(file, "Tämä on väliaikainen tiedosto")
```

Voit myös määrittää halutun hakemiston, johon väliaikainen tiedosto tallennetaan, seuraavasti:

```Elixir
{:ok, file} = File.open_temp("temp.txt", ["tmp"])
```

Kun olet valmis käsittelemään tiedostoa, voit sulkea sen normaalisti käyttämällä ```File.close/1``` -funktiota.

## Syvemmälle

Elixirissä väliaikaiset tiedostot luodaan käyttämällä Erlangin ```:os.make_temp_dir/1``` ja ```:filelib.format_filename/2``` -funktioita. Näitä voidaan myös käyttää, jos haluat enemmän hallintaa väliaikaisten tiedostojen luomisessa.

Voit määrittää halutun tiedostopäätteen ja hakemiston, jossa väliaikainen tiedosto tallennetaan, käyttämällä seuraavia argumentteja:

```Elixir
{:ok, file} = File.open_temp("temp", ["txt"], "tmp")
```

Lisätietoja Elixirin väliaikaisten tiedostojen luomisesta löytyy [virallisesta dokumentaatiosta](https://hexdocs.pm/elixir/File.html#open_temp/2).

## Katso myös

- [Virallinen Elixir-dokumentaatio](https://hexdocs.pm/elixir/File.html#open_temp/2)
- [Erlangin os.make_temp_dir/1 -funktio](http://erlang.org/doc/man/os.html#make_temp_dir-1)
- [Erlangin filelib.format_filename/2 -funktio](http://erlang.org/doc/man/filelib.html#format_filename-2)