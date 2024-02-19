---
aliases:
- /fi/ruby/using-a-debugger/
date: 2024-01-26 04:09:50.159908-07:00
description: "Rubyssa debuggerin k\xE4ytt\xF6 antaa ohjelmoijille supervoiman pys\xE4\
  ytt\xE4\xE4 koodinsa, tarkastella muuttujia ja k\xE4yd\xE4 l\xE4pi koodiaan rivi\
  \ rivilt\xE4. Ihmiset tekev\xE4t\u2026"
lastmod: 2024-02-18 23:09:08.187723
model: gpt-4-0125-preview
summary: "Rubyssa debuggerin k\xE4ytt\xF6 antaa ohjelmoijille supervoiman pys\xE4\
  ytt\xE4\xE4 koodinsa, tarkastella muuttujia ja k\xE4yd\xE4 l\xE4pi koodiaan rivi\
  \ rivilt\xE4. Ihmiset tekev\xE4t\u2026"
title: "Debuggerin k\xE4ytt\xF6"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Rubyssa debuggerin käyttö antaa ohjelmoijille supervoiman pysäyttää koodinsa, tarkastella muuttujia ja käydä läpi koodiaan rivi riviltä. Ihmiset tekevät sitä virheiden korjaamiseksi, koodin kulun ymmärtämiseksi ja nähdäkseen tarkalleen, mitä heidän kirjoittamansa loitsut (koodi) tekevät, kun taika tapahtuu – tai ei tapahdu.

## Kuinka:

Ruby sisältää sisäänrakennetun debuggerin nimeltä `byebug`. Lisää ensin `byebug` Gemfile-tiedostoosi ja suorita `bundle install`. Sitten, pudota `byebug` juuri siihen kohtaan ohjelmaasi, missä haluat ohjelmasi hengähtävän.

```Ruby
require 'byebug'

def calculate_magic(number)
  byebug
  magic_number = number * 7
  return magic_number
end

puts calculate_magic(6)
```

Tämän skriptin suorittaminen pysäyttää suorituksen kohtaan `byebug`, ja sinut heitetään interaktiiviseen istuntoon, jossa voit kirjoittaa komentoja, kuten:

```
step
next
continue
var local
```

Esimerkkituloste antaisi sinulle kehotteen, joka näyttää tältä:

```
[2, 11] in example.rb
    2: 
    3: def calculate_magic(number)
    4:   byebug
=>  5:   magic_number = number * 7
    6:   return magic_number
    7: end
    8: 
    9: puts calculate_magic(6)
(byebug)
```

## Syvä sukellus:

Kauan ennen `byebug`ia, Rubyistit käyttivät `debugger`ia ja `pry`ä. Jälkimmäinen, `pry`, on enemmän kuin debuggeri; se on tehokas REPL, jota voidaan myös käyttää debuggauksessa `binding.pry`-katkaisukohdan avulla.

Vaihtoehtoja Rubyn `byebug`ille sisältävät `pry-byebug`, joka yhdistää `pry`n ja `byebug`in toiminnallisuudet, ja `ruby-debug`, joka on vanhempi gem eikä sitä ylläpidetä aktiivisesti.

Kun kutsut `byebug`ia, debuggeri keskeyttää koodisi suorituksen ja antaa sinulle kurkistuksen suoritukseen. Voit nähdä ja muuttaa muuttujia, hypätä eri kohtiin koodissa ja jopa suorittaa joitakin Ruby-koodirivejä riviltä. Se on vähän kuin aikamatkailukyvyn omaaminen Ruby-koodillesi.

## Katso myös:

- Byebug GitHub-repositorio: [https://github.com/deivid-rodriguez/byebug](https://github.com/deivid-rodriguez/byebug)
- Pry dokumentaatio: [https://github.com/pry/pry](https://github.com/pry/pry)
- Opas Rails-sovellusten debuggaamiseen: [https://guides.rubyonrails.org/debugging_rails_applications.html](https://guides.rubyonrails.org/debugging_rails_applications.html)
