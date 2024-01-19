---
title:                "Merkkijonon suuraakkostaminen"
html_title:           "Ruby: Merkkijonon suuraakkostaminen"
simple_title:         "Merkkijonon suuraakkostaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Pikkukirjaimen korvaaminen isolla kirjaimella Rubyssa: Miksi ja Miten?

## Mikä & Miksi?
Kirjaimen suurentaminen eli "capitalizing" tarkoittaa pikkukirjaimen muuttamista isolla kirjaimellä. Ohjelmoijat tekevät tämän usein tekstin esittämiseksi paremmin - esimerkiksi, nimet usein aloitetaan isolla kirjaimella.

## Miten:
Ruby tekee tämän helpoksi `.capitalize` -metodin avulla. Se muuttaa merkkijonon ensimmäisen kirjaimen isoksi ja muut pieniksi.

```Ruby
name = "kalle"
puts name.capitalize
```

Tämän koodinpätkän tuloste on `Kalle`. Helppoa, eikö niin?

## Syvällisempi katsaus
Historiallisessa kontekstissa, iso kirjain markkinoi erityisesti nimiä tai aloituslauseita. Rubyssa `.capitalize` on peräisin alkuperäisestä String-luokasta, jonka vuoksi sen tyypillinen käyttö on säilynyt samanlaisena.

Tietysti on myös vaihtoehtoja. `.upcase` muuttaa kaikki kirjaimet isoiksi, ja `.downcase` muuttaa kaikki kirjaimet pieniksi. Kuitenkin, `.capitalize` on ainutlaatuinen, koska se tekee molempia: Muuntaa ensimmäisen kirjaimen isoksi ja loput pieniksi.

`.capitalize` toteutetaan sisäisesti verraten verraten ensimmäistä merkkiä ASCII-koodistossa ja muuttamalla se isoksi, jos se on pieni.

## Katso myös
1. Ruby String capitalize-metodi: [https://www.rubyguides.com/2018/10/ruby-string-methods/](https://www.rubyguides.com/2018/10/ruby-string-methods/)
2. ASCII-koodiston taulukko: [https://www.asciitable.com/](https://www.asciitable.com/)
3. Lyhyt opas Ruby String-metodeille: [https://mixandgo.com/learn/25-ruby-string-methods-you-should-know](https://mixandgo.com/learn/25-ruby-string-methods-you-should-know)