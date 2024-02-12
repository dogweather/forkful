---
title:                "Virheenjäljitystulosteiden tulostaminen"
aliases:
- /fi/ruby/printing-debug-output.md
date:                  2024-01-20T17:53:33.494522-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheenjäljitystulosteiden tulostaminen"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/printing-debug-output.md"
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
