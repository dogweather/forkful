---
date: 2024-01-20 17:53:33.494522-07:00
description: "How to: - N\xE4in teet: Aikoinaan koodarin ty\xF6kalupakissa `puts`\
  \ ja `p` olivat peruskeinoja n\xE4hd\xE4, mit\xE4 koodissa tapahtuu. Verrattuna\
  \ moderniin\u2026"
lastmod: '2024-04-05T22:51:11.231378-06:00'
model: gpt-4-1106-preview
summary: "- N\xE4in teet: Aikoinaan koodarin ty\xF6kalupakissa `puts` ja `p` olivat\
  \ peruskeinoja n\xE4hd\xE4, mit\xE4 koodissa tapahtuu. Verrattuna moderniin debuggereihin,\
  \ kuten `byebug` tai `pry`, n\xE4m\xE4 ovat primitiivisi\xE4, mutta toisinaan ne\
  \ ovat nopein tapa saada yleiskuva ongelmasta. `puts` tulostaa muuttujat ja lausekkeet\
  \ muunnettuna merkkijonoiksi, kun taas `p` k\xE4ytt\xE4\xE4 `inspect`-metodia, joka\
  \ antaa yksityiskohtaisempaa tietoa objektista. K\xE4yt\xE4 `puts`ia kun tarvitset\
  \ siistit, ihmisen luettavat tulostukset ja `p`:t\xE4 kun tarvitset teknisemp\xE4\
  \xE4 dataa. Ruby on sis\xE4isesti optimoitu k\xE4sittelem\xE4\xE4n suoria tulostuksia\
  \ hyvin, joten debug-tulostuksen k\xE4ytt\xF6 ei yleens\xE4 hidasta ohjelmaa merkitt\xE4\
  v\xE4sti kehitysvaiheessa."
title: "Virheenj\xE4ljitystulosteiden tulostaminen"
weight: 33
---

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
