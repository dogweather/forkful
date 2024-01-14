---
title:    "Elixir: Tarkistetaan, onko hakemisto olemassa"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi
Monissa ohjelmointiprojekteissa voi olla tarve tarkistaa, onko tietty hakemisto olemassa. Tämä on erityisen tärkeää, jos haluat varmistaa, että tietyn tiedoston tallentaminen tai lukeminen on mahdollista.

## Kuinka
Voit tarkistaa hakemiston olemassaolon Elixirin `File`-moduulin avulla. Käytä `File.exists?` -funktiota ja anna funktiolle hakemiston polku parametrina. Tässä on esimerkki koodi, joka tarkistaa, onko `logs`-hakemisto olemassa:

```Elixir
if File.exists?("logs") do
  IO.puts "Logs-hakemisto löytyi!"
else
  IO.puts "Logs-hakemisto ei ole käytettävissä."
end
```

Tämän koodin tulostuksena näet joko "Logs-hakemisto löytyi!" tai "Logs-hakemisto ei ole käytettävissä." riippuen siitä, onko `logs`-hakemisto olemassa vai ei.

## Syvällisempi tarkastelu
Tarkastellessamme `File.exists?` funktiota tarkemmin, huomaamme, että se palauttaa arvon `true` jos hakemisto on olemassa ja `false` jos sitä ei löydy. Tämän avulla voit tehdä lisätoimintoja sen perusteella, onko tietty hakemisto käytettävissä vai ei.

`File`-moduulin lisäksi voit myös käyttää Elixirin `Path`-moduulia tarkistaaksesi hakemiston olemassaolon. `Path.expand/2` -funktio laajentaa polun ja `Path.wildcard?/1`-funktio palauttaa arvon `true` jos polku vastaa hakemiston nimeä.

## Katso myös
- [Elixir File-moduulin dokumentaatio](https://hexdocs.pm/elixir/File.html)
- [Elixir Path-moduulin dokumentaatio](https://hexdocs.pm/elixir/Path.html)
- [Kuinka luoda hakemistoja Elixirissa](https://blog.appsignal.com/2018/07/03/how-to-create-a-directory-in-elixir.html)