---
title:                "Debug-tulosteen tulostaminen"
html_title:           "Bash: Debug-tulosteen tulostaminen"
simple_title:         "Debug-tulosteen tulostaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

Tulostusjäljitys on ohjelmistosuunnittelun työkalu, jolla voidaan tulostaa tilapäisiä tekstiviestejä, jotka auttavat selvittämään koodin toimintaa. Ohjelmoijat käyttävät sitä virheiden löytämiseksi ja niiden korjaamiseksi.

## Miten:

Rubyssa voit tulostaa debug-lausunnon käyttämällä `puts`, `print` tai `p` - komentoja. Tässä esimerkki:

```Ruby
def sum(x, y)
  result = x + y
  puts "sum is: #{result}"
  result
end
total = sum(5, 10)
```

Esimerkin tulostus näyttäisi tältä:

```Ruby
"sum is: 15"
```

## Syvällisempää Tietoa

1. Historiallinen Konteksti: Jäljitystulostus on syntynyt ohjelmoinnin alkuaikoina, kun ohjelmoijat tarvitsivat keinoja seurata ohjelman toimintaa reaaliaikaisesti. Tätä työkalua on sen jälkeen parannettu ja mukautettu moneen eri ohjelmointikielillä.

2. Vaihtoehdot: Ruby tarjoaa myös 'debugger' -gem, joka tarjoaa kehittyneitä jäljitystyökaluja, kuten katkaisupisteitä ja koodin askellusta.

3. Toteutuksen yksityiskohdat: 'puts' käyttää Rubyssa olevaa $stdout-objektia ja lisää aina rivinvaihdon loppuun. 'print' käyttää myös $stdout-objektia, mutta ei lisää rivinvaihtoa. 'p' on käytännöllinen, koska se lisää rivinvaihdon ja kutsuu inspect-metodia objekteilla, mikä antaa erityiskohtaisemman kuvauksen objektista.

## Katso myös 

1. '[Debugging with Pry](https://www.rubyguides.com/2020/04/debugging-ruby-with-pry/)': Kattava opas käyttämällä Pry komentoa Rubyssa.
2. '[Ruby Debugging Magic Cheat Sheet](https://www.rubyguides.com/2015/06/ruby-debugging-magic-cheat-sheet/)': Cheat Sheet käsinti jäljitys -määritteille Rubyssa.
3. '[Debugging Ruby: Understanding Ruby and Rails Logging](https://www.toptal.com/ruby/debugging-ruby-understanding-ruby-and-rails-logging)': Toinen tapa debugata ohjelmia on käyttää lokitietojen kirjoittamista Rubyssa ja Railsissa. Tämä opas tarjoaa lisätietoja aiheesta.