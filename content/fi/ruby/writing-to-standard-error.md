---
title:                "Tallentaminen standardivirheeseen"
html_title:           "Ruby: Tallentaminen standardivirheeseen"
simple_title:         "Tallentaminen standardivirheeseen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

**Mikä & Miksi?**

Kirjoittaminen standardivirheeseen on yksinkertainen tapa näyttää ohjelman virhetilanteet. Monet ohjelmoijat käyttävät tätä tapaa tunnistaakseen ja korjatakseen koodissa olevia virheitä. 

**Kuinka:**

```ruby
puts "Tämä tulostetaan standardilähtöön"

$stderr.puts "Tämä tulostetaan standardivirheeseen"
```

**Syvä Syvennys:**

Kirjoittaminen standardivirheeseen on yleinen tapa käsitellä virheitä Ruby-ohjelmoinnissa. Ennen standardilähtön käyttöönottoa, standardivirhe oli ainoa keino tunnistaa ja korjata virheitä ohjelmassa. Nykyään on olemassa myös muita tapoja käsitellä virheitä, kuten exception-käsittely. Standardivirheellä on kuitenkin edelleen tärkeä rooli koodin virheitä korjattaessa.

**Katso myös:**

[RubyDocs: https://ruby-doc.org/core-3.0.0/IO.html#method-c-e](https://ruby-doc.org/core-3.0.0/IO.html#method-c-e)

[RubyGuides: https://www.rubyguides.com/2019/05/working-with-standard-errors/](https://www.rubyguides.com/2019/05/working-with-standard-errors/)