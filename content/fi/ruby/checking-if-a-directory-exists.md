---
title:                "Onko hakemisto olemassa? Tarkistaminen"
date:                  2024-01-20T14:58:11.792989-07:00
html_title:           "Gleam: Onko hakemisto olemassa? Tarkistaminen"
simple_title:         "Onko hakemisto olemassa? Tarkistaminen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Tarkistetaan, onko hakemisto olemassa, jotta vältetään virheet tiedostoihin kirjoitettaessa tai hakemistosta luettaessa. Tämä vahvistaa koodin luotettavuutta, koska varmistetaan, että operoitava kohde on olemassa.

## Kuinka toimia:
```Ruby
require 'fileutils'

# Tarkista, onko hakemisto olemassa
if Dir.exist?('/polku/hakemistoon')
  puts "Hakemisto on olemassa."
else
  puts "Hakemisto ei ole olemassa, luodaan..."
  FileUtils.mkdir_p('/polku/hakemistoon')
end
```
Jos hakemisto on olemassa:
```
Hakemisto on olemassa.
```
Jos hakemistoa ei ole:
```
Hakemisto ei ole olemassa, luodaan...
```

## Syväsukellus:
Ruby on perinteisesti käyttänyt `File`- ja `Dir`-luokkia tiedostojen ja hakemistojen käsittelyssä. `Dir.exist?` on suora tapa tarkistaa hakemiston olemassaolo, kun taas `File.directory?` on vanhempi, vaihtoehtoinen metodi. Koodin selkeyden vuoksi suositellaan käyttämään `Dir.exist?` kun kyse on nimenomaan hakemistoista. Raaka polkujen käsittely voi aiheuttaa ongelmia eri käyttöjärjestelmien välillä, mutta Ruby pyrkii piilottamaan nämä yksityiskohdat abstraktion tasolla.

## Katso myös:
- Ruby FileUtils moduuli: https://ruby-doc.org/stdlib-2.5.1/libdoc/fileutils/rdoc/FileUtils.html
- Ruby Dir luokka: https://ruby-doc.org/core-2.5.1/Dir.html
- Ruby File luokka: https://ruby-doc.org/core-2.5.1/File.html
