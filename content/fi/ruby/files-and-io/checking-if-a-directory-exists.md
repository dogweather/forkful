---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:15.570706-07:00
description: "Miten: Rubyn vakio kirjasto tarjoaa suoraviivaiset menetelm\xE4t tarkistaa\
  \ hakemiston olemassaolo. N\xE4in teet sen puhtaalla Rubylla, ilman kolmannen osapuolen\u2026"
lastmod: '2024-03-13T22:44:57.101631-06:00'
model: gpt-4-0125-preview
summary: "Rubyn vakio kirjasto tarjoaa suoraviivaiset menetelm\xE4t tarkistaa hakemiston\
  \ olemassaolo."
title: Tarkistetaan, onko hakemisto olemassa
weight: 20
---

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
