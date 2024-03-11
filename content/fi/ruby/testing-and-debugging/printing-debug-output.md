---
date: 2024-01-20 17:53:33.494522-07:00
description: "Debug-tulostus on koodissa olevien `puts` tai `p` -komentojen k\xE4\
  ytt\xF6\xE4 v\xE4liaikaisten viestien n\xE4ytt\xE4miseen, jotta n\xE4hd\xE4\xE4\
  n mit\xE4 ohjelmassa tapahtuu. Se\u2026"
lastmod: '2024-03-11T00:14:31.130784-06:00'
model: gpt-4-1106-preview
summary: "Debug-tulostus on koodissa olevien `puts` tai `p` -komentojen k\xE4ytt\xF6\
  \xE4 v\xE4liaikaisten viestien n\xE4ytt\xE4miseen, jotta n\xE4hd\xE4\xE4n mit\xE4\
  \ ohjelmassa tapahtuu. Se\u2026"
title: "Virheenj\xE4ljitystulosteiden tulostaminen"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
Debug-tulostus on koodissa olevien `puts` tai `p` -komentojen käyttöä väliaikaisten viestien näyttämiseen, jotta nähdään mitä ohjelmassa tapahtuu. Se auttaa koodareita ymmärtämään ohjelman käyttäytymistä ja löytämään bugeja nopeasti.

## How to: - Näin teet:
```Ruby
# Yksinkertainen esimerkki
def laske_summa(a, b)
  summa = a + b
  puts "Summa: #{summa}" # Debug-tulostus
  summa
end

puts laske_summa(2,3)
# Tulostaa: Summa: 5
# Ja sen jälkeen: 5

# Monimutkaisempi esimerkki objektin sisällä
class Laskuri
  def initialize
    @laskuri = 0
  end

  def kasvata
    @laskuri += 1
    p @laskuri # Debug-tulostus käyttäen 'p' -metodia
  end
end

laskuri = Laskuri.new
laskuri.kasvata
# Tulostaa: 1
```

## Deep Dive - Syväsukellus:
Aikoinaan koodarin työkalupakissa `puts` ja `p` olivat peruskeinoja nähdä, mitä koodissa tapahtuu. Verrattuna moderniin debuggereihin, kuten `byebug` tai `pry`, nämä ovat primitiivisiä, mutta toisinaan ne ovat nopein tapa saada yleiskuva ongelmasta. 

`puts` tulostaa muuttujat ja lausekkeet muunnettuna merkkijonoiksi, kun taas `p` käyttää `inspect`-metodia, joka antaa yksityiskohtaisempaa tietoa objektista. Käytä `puts`ia kun tarvitset siistit, ihmisen luettavat tulostukset ja `p`:tä kun tarvitset teknisempää dataa.

Ruby on sisäisesti optimoitu käsittelemään suoria tulostuksia hyvin, joten debug-tulostuksen käyttö ei yleensä hidasta ohjelmaa merkittävästi kehitysvaiheessa.

## See Also - Katso myös:
- Ruby dokumentaatio `puts`: http://ruby-doc.org/core-2.7.0/IO.html#method-i-puts
- Ruby dokumentaatio `p`: http://ruby-doc.org/core-2.7.0/Kernel.html#method-i-p
- Byebug, hyödyllinen Ruby debuggeri: https://github.com/deivid-rodriguez/byebug
- Pry, interaktiivinen komentokehoite Rubyyn: http://pryrepl.org/
