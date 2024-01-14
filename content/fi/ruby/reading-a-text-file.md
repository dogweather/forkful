---
title:                "Ruby: Tiedoston lukeminen"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tervetuloa lukemaan blogipostausta Ruby-ohjelmoinnista! Tässä artikkelissa keskitymme siihen, miten voit lukea tekstifileitä Ruby-kielellä.

Usein tekstifilejä sisältävät tiedostot ovat olennainen osa ohjelmointia ja niiden käsittelyyn liittyy monia käytännön tilanteita, kuten käyttäjän asetukset, tietokannan taulukot tai vain tekstin tallentaminen ja tulostaminen. Siksi onkin tärkeää, että osaat lukea tekstifilejä Ruby-ohjelmoinnin seuraavalla tasolla.

## Miten

Kirjoita seuraava koodi Ruby-tiedostoon `read_text_file.rb`:

```Ruby
require 'fileutils'

# avaaminen ja lukeminen tiedostosta
file = File.open('tekstitiedosto.txt', 'r')
puts file.read

# rivien lukeminen iteratiivisesti
file.each_line do |line|
  puts line
end

# tiedoston sulkeminen
file.close

# olemassa olevan tiedoston kirjoittaminen
File.write('kirjoitus.txt', 'Tämä on esimerkki tekstistä.')

# tiedostojen kopiointi
FileUtils.cp('tekstifile.txt', 'kopio.txt')
```

Yllä olevassa koodissa käytetään Ruby:n `File`-luokkaa avaamaan ja lukemaan tiedostoja. Voit käyttää `read`-metodia lukeaksesi koko tiedoston sisällön tai `each_line`-metodia lukemaan tiedoston riveittäin. Muista myös sulkea tiedosto käytön jälkeen.

Lisäksi esitetään esimerkki, miten voit kirjoittaa uuden tiedoston tai kopioida olemassa olevan tiedoston käyttäen `File`- ja `FileUtils`-luokkia. Nämä ovat vain muutamia esimerkkejä tavanomaisista tekstifileillä työskentelytavoista.

Suorita yllä oleva koodi ja näet tekstin tulostettuna terminaaliisi. Voit myös luoda oman tekstifilesi ja kokeilla eri tapoja lukea, kirjoittaa ja kopioida tekstejä.

## Syventävä tarkastelu

Vaikka artikkelissa käsiteltiinkin vain muutamia esimerkkejä, tekstin lukeminen ja käsittely Rubyllä voi olla monimutkaisempaa kuin näyttää. Voit tutustua lisää Ruby:n `File`- ja `FileUtils`-luokkiin ja niiden tarjoamiin mahdollisuuksiin.

Lisäksi voit myös perehtyä erilaisiin tapoihin käsitellä tekstin koodauksia ja erilaisia muotoilumahdollisuuksia, kuten Markdownin tai HTML:n luominen tiedoille.

## Katso myös

- [Ruby:n viralliset dokumentaatiot File-luokasta](https://ruby-doc.org/core-2.7.0/File.html)
- [Ruby:ta käsittelevä tutoriaali tekstin käsittelystä](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
- [Ruby:sta löytyvä muita hyviä esimerkkejä tekstin kirjoittamiseen ja kopiointiin](https://www.techotopia.com/index.php/Working_with_Text_Files_in_Ruby)