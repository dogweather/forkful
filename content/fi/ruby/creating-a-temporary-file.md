---
title:                "Tilapäistiedoston luominen"
html_title:           "Ruby: Tilapäistiedoston luominen"
simple_title:         "Tilapäistiedoston luominen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Luodessaan väliaikaisen tiedoston, ohjelmoijat haluavat tallentaa tilapäisesti tietoja ennen niiden pysyvää tallentamista. Tätä käytetään usein esimerkiksi tiedostojen käsittelyssä tai tietokantojen muokkaamisessa.

## Kuinka teet sen:
```ruby
require "tempfile"

temp_file = Tempfile.new("temp_file") # luo väliaikaisen tiedoston nimellä "temp_file"
temp_file.write("This is a temporary file.") # kirjoittaa tiedostoon
temp_file.rewind # palauttaa tiedoston alkuun
puts temp_file.read # tulostaa tiedoston sisällön
temp_file.close # sulkee tiedoston

=> "This is a temporary file."
```

## Syvempi sukellus:
Väliaikaisten tiedostojen luonti on ollut osa ohjelmointia jo pitkään, ja sitä käytetään edelleen laajasti. Useimmissa tapauksissa väliaikaiset tiedostot tuhotaan automaattisesti niiden käytön jälkeen, mutta joissakin tapauksissa ne voidaan myös tallentaa pysyvästi. On myös muita tapoja tallentaa väliaikaisia tietoja, kuten käyttämällä väliaikaisia muuttujia tai muistialueita.

## Katso myös:
- https://ruby-doc.org/stdlib-2.7.1/libdoc/tempfile/rdoc/Tempfile.html
- https://www.geeksforgeeks.org/ruby-tempfile-class/
- https://www.rubyguides.com/2018/11/temporary-files-in-ruby/