---
title:    "Ruby: Tarkistetaan, onko hakemistoa olemassa"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi: Tarkista hakemiston olemassaolo

On monia syitä, miksi haluaisit tarkistaa, onko tietyllä hakemistolla olemassaoleva. Saattaa olla, että ohjelmasi tarvitsee tiettyä hakemistoa toimiakseen, tai ehkä haluat varmistaa, että hakemisto on olemassa ennen kuin tallennat sinne tiedostoja. Tämä on erittäin tärkeä osa ohjelmointia, ja siksi tarkistamme nyt, miten tämä tehdään Rubylla.

## Miten: Koodiesimerkkejä ja tulosteen esittelyä

```ruby
# Tarkista, onko hakemisto olemassa

if Dir.exist?("hakemisto_nimi")
  puts "Hakemisto on olemassa!"
else
  puts "Hakemistoa ei löytynyt."
end
```

Tässä yksinkertaisessa koodiesimerkissä käytämme Ruby-metodia `Dir.exist?`, joka tarkistaa parametrina annetun hakemiston olemassaolon. Sen jälkeen tulostetaan ilmoitus, riippuen siitä, onko hakemisto olemassa vai ei. Voit myös käyttää `Dir.exists?`-metodia, joka toimii samalla tavalla.

```ruby
# Luo uusi hakemisto ja tarkista sen olemassaolo

Dir.mkdir("uusi_hakemisto")
if Dir.exist?("uusi_hakemisto")
  puts "Uusi hakemisto luotu ja se on olemassa."
else
  puts "Hakemistoa ei löytynyt."
end
```

Tässä toisessa esimerkissä luomme uuden hakemiston käyttämällä `Dir.mkdir`-metodia ja sitten tarkistamme sen olemassaolon `Dir.exist?`-metodilla. Voit kokeilla näitä esimerkkejä omassa Ruby-ympäristössäsi ja näet, miten ne toimivat.

## Syventävä sukellus

Rubyssa on myös muita tapoja tarkistaa hakemiston olemassaolo, kuten `FileTest.exist?(path)`-metodi, joka voi tarkistaa olemassaolon millä tahansa tiedostopolulla. Voit myös käyttää `File.directory?(path)`-metodia, joka tarkistaa, onko annettu polku hakemisto vai ei.

On myös tärkeää huomata, että tarkistamalla hakemiston olemassaoloa, koodin suoritus voi hidastua, jos hakemisto on suuri tai jos tiedostojärjestelmä on hidas. Käytä siis harkintaa ja hienosäädä koodiasi, jos teet toistuvia tarkistuksia hakemiston olemassaololle.

## Katso myös

- [Ruby Dir-luokka](https://ruby-doc.org/core-2.7.2/Dir.html)
- [Ruby FileTest-moduuli](https://ruby-doc.org/core-2.7.2/FileTest.html)
- [Ruby tiedostojärjestelmätoiminnot](https://www.rubyguides.com/ruby-tutorial/working-with-files/)