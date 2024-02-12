---
title:                "Tarkistetaan, onko hakemisto olemassa"
aliases:
- fi/ruby/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:15.570706-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mitä & Miksi?
Tarkistamalla, onko hakemisto olemassa Rubylla, ohjelmoijat voivat varmistaa hakemiston olemassaolon ennen toimintojen, kuten tiedostojen lukemisen tai uusien hakemistojen luomisen, suorittamista. Tämä on ratkaisevan tärkeää välttääkseen virheitä tiedostojen käsittelyssä ja varmistaakseen tiedostojärjestelmän manipulointien luotettavuuden.

## Miten:
Rubyn vakio kirjasto tarjoaa suoraviivaiset menetelmät tarkistaa hakemiston olemassaolo. Näin teet sen puhtaalla Rubylla, ilman kolmannen osapuolen kirjastoja:

```ruby
require 'fileutils'

# Tarkista, onko hakemisto olemassa
if Dir.exist?('/polku/hakemistoon')
  puts 'Hakemisto on olemassa.'
else
  puts 'Hakemistoa ei ole olemassa.'
end
```
Esimerkkituloste:
```
Hakemisto on olemassa.
```
Tai:
```
Hakemistoa ei ole olemassa.
```

`Dir.exist?`-metodin lisäksi voit myös käyttää `File.directory?`-metodia, joka palauttaa `true`, jos annettu polku on hakemisto:

```ruby
if File.directory?('/polku/hakemistoon')
  puts 'Hakemisto on olemassa.'
else
  puts 'Hakemistoa ei ole olemassa.'
end
```
Sekä `Dir.exist?` että `File.directory?` ovat osa Rubyn vakio kirjastoa eivätkä vaadi ulkopuolisia gemmejä käyttöönsä, mikä tekee niistä käteviä ja tehokkaita vaihtoehtoja hakemistojen tarkistuksille.
