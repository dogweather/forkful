---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Elixir: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Tarkistamalla onko tietty hakemisto olemassa, voi varmistaa tiedoston sijainnin. Tämä on tärkeää ohjelmiston toiminnan kannalta, koska se estää virheitä, jotka johtuvat tiedostojen puuttumisesta tai niiden sijaintien virheellisyydestä.

## Näin se tehdään:
Elixirin File-moduuli tarjoaa `exists?` -funktion tarkistamaan, onko tietty hakemisto olemassa. Esimerkki sen käytöstä:

```elixir
if File.dir?("path/to/directory") do
  IO.puts "Hakemisto on olemassa."
else
  IO.puts "Hakemisto ei ole olemassa."
end
```

Tuloste riippuu siitä, onko hakemisto olemassa vai ei:

```
Hakemisto on olemassa.
```
Tai
```
Hakemisto ei ole olemassa.
```

## Syvemmälle
Ennen File-moduulin saapumista Elixirissä, yleinen tapa tarkistaa, onko hakemisto olemassa, oli käyttää Erlangin :file.file_info -funktiota. Tämä menetelmä on yhä voimassa, mutta Elixirin `File.dir?` on ilmaisullisempi ja helpompi ymmärtää aloittelijoille.

Vaihtoehtoisesti voidaan käyttää `File.ls/1` -funktiota tarkistamiseen, mutta tämä on törmää virheeseen, jos hakemistoa ei ole olemassa, joten sen käyttö ei ole suositeltavaa tässä tilanteessa.

Tarkistus toimii kysymällä käyttöjärjestelmältä, onko annetun polun kohde olemassa ja onko se hakemisto. Tämä prosessi on erittäin nopea ja virheetön, sillä se perustuu alhaiseen tasoon kirjastoihin, jotka on suunniteltu tätä tarkoitusta varten. 

## Katso myös
[List of functions in File module in Elixir](https://hexdocs.pm/elixir/File.html#content): Täydellinen luettelo kaikista File-moduulin funktioista Elixirissä, mukaan lukien `dir?`.

[Erlang :file module documentation](http://erlang.org/doc/man/file.html#file_info-1): dokumentaatio :file-moduulista Erlangissa, jota käytettiin aikaisemmin.