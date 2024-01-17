---
title:                "Yamlin kanssa työskentely"
html_title:           "Ruby: Yamlin kanssa työskentely"
simple_title:         "Yamlin kanssa työskentely"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
YAML on ihmisen luettava tietoformaatti, joka on suosittu ohjelmoijien keskuudessa. Sitä käytetään tallentamaan ja siirtämään tietoja, kuten asetuksia ja rakenteita, käyttäen helppolukuista syntaksia. Tämä tekee siitä suositun vaihtoehdon esimerkiksi XML:lle.

## Kuinka:
Tässä on muutamia esimerkkejä siitä, kuinka voit käyttää Rubya YAML:in kanssa.

```ruby
require 'yaml'

# Tallennetaan hash tekstitiedostoon
settings = { 
  color: "red", 
  font_size: 12 
}

File.open("asetukset.yaml", "w") do |f| 
  YAML.dump(settings, f) 
end 

# Luetaan tekstitiedosto takaisin hash muotoon
asetukset = YAML.load(File.read("asetukset.yaml"))
puts asetukset["color"] # "red"
puts asetukset["font_size"] # 12
```

## Syvällinen sukellus:
YAML kehitettiin alun perin vuonna 2001, ja se on lyhenne sanoista "YAML Ain't Markup Language". Sittemmin siitä on tullut suosittu vaihtoehto muille tietoformaateille, kuten JSON ja XML. YAML on myös käytössä mm. Ruby on Rails -kehyksessä ja Githubin konfiguraatiotiedostoissa.

## Katso myös:
- [YAML.org](https://yaml.org/)
- [Ruby dokumentaatio](https://ruby-doc.org/stdlib-2.7.1/libdoc/yaml/rdoc/YAML.html) 
- [Rails dokumentaatio](https://guides.rubyonrails.org/configuring.html#configuration-and-customization)