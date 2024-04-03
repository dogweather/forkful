---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:09.683563-07:00
description: "Kuinka: Elixiring vakiokirjasto tarjoaa suoraviivaisen tavan tarkistaa\
  \ hakemiston olemassaolo `File`-moduulin kautta. N\xE4in voit k\xE4ytt\xE4\xE4 sit\xE4\
  ."
lastmod: '2024-03-13T22:44:56.241514-06:00'
model: gpt-4-0125-preview
summary: Elixiring vakiokirjasto tarjoaa suoraviivaisen tavan tarkistaa hakemiston
  olemassaolo `File`-moduulin kautta.
title: Tarkistetaan, onko hakemisto olemassa
weight: 20
---

## Kuinka:
Elixiring vakiokirjasto tarjoaa suoraviivaisen tavan tarkistaa hakemiston olemassaolo `File`-moduulin kautta. Näin voit käyttää sitä:

```elixir
if File.dir?("polku/hakemistoon") do
  IO.puts "Hakemisto on olemassa!"
else
  IO.puts "Hakemistoa ei ole olemassa."
end
```

Esimerkkitulostus, olettaen että hakemistoa ei ole olemassa:
```
Hakemistoa ei ole olemassa.
```

Edistyneempiin tiedostojärjestelmän interaktioihin, mukaan lukien hakemiston olemassaolon tarkistaminen, saatat harkita kolmansien osapuolien kirjastojen, kuten `FileSystem`, käyttämistä. Vaikka Elixiring vakiotoiminnot riittävät monissa tapauksissa, `FileSystem` voi tarjota tarkempaa hallintaa ja palautetta monimutkaisten sovellusten kannalta. Kuitenkin perustarpeeseen tarkistaa, onko hakemisto olemassa, on yleensä suositeltavaa pysytellä natiivissa `File`-moduulissa, koska se on helposti saatavilla eikä vaadi ulkoisia riippuvuuksia.
