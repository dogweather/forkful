---
title:                "Tekstitiedoston kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
"Mitä & Miksi?"

Tekstitiedoston kirjoittaminen Rubyssä tarkoittaa tiedon tallentamista levylle tekstiformaatissa. Ohjelmoijat tekevät tämän datan varmuuskopiointia, lokitietojen seurantaa tai käyttäjätietojen talteenottoa varten.

## How to:
"Kuinka tehdä:"

```Ruby
# Tiedoston luominen ja kirjoittaminen
File.open("esimerkki.txt", "w") do |file|
  file.puts "Hei Ruby!"
end

# Tiedoston lukeminen
sisalto = File.read("esimerkki.txt")
puts sisalto
```

Tuloste:
```
Hei Ruby!
```

## Deep Dive
"Sukellus syvyyksiin"

Ennen Rubyä tiedostojen käsittely oli yleensä monimutkaisempaa. Historiallisesti esimerkiksi C-kielessä käytettiin FILE-pohjaista lähestymistapaa. Ruby tarjoaa korkeamman tason IO-luokan, joka helpottaa tiedostonkäsittelyä. Vaihtoehtoisesti voi käyttää `IO#write` ja muita IO-menetelmiä tai kirjastoja, kuten CSV tai YAML tiedostoille. Implementation yksityiskohtien ymmärtäminen auttaa parantamaan suorituskykyä ja tietoturvaa.

## See Also
"Näytä Myös"

- Ruby IO Documentation: https://ruby-doc.org/core-3.1.0/IO.html
- File class Documentation: https://ruby-doc.org/core-3.1.0/File.html
- Ruby-Doc File I/O: https://www.ruby-doc.org/core-3.1.0/doc/syntax/io_rdoc.html
