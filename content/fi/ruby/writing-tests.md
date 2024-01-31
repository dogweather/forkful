---
title:                "Testien kirjoittaminen"
date:                  2024-01-19
simple_title:         "Testien kirjoittaminen"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Testaus on koodin varmistusmetodi: se tarkistaa, että koodi toimii odotetusti. Testit nopeuttavat kehitystä, vähentävät bugeja ja helpottavat ylläpitoa.

## How to:

Rubyssa testien kirjoittaminen on suoraviivaista, erityisesti kun käytetään RSpec-kirjastoa. Asenna RSpec komennolla `gem install rspec` ja seuraa alla olevia esimerkkejä.

```Ruby
# spec/calculator_spec.rb
RSpec.describe 'Calculator' do
  it 'summaa kaksi numeroa oikein' do
    expect(2 + 2).to eq(4)
  end
  
  it 'vähentää oikein' do
    expect(5 - 3).to eq(2)
  end
end
```
Ajetaan testit komennolla `rspec spec/calculator_spec.rb`.
Tuloste:
```
...

Finished in 0.00276 seconds (files took 0.15708 seconds to load)
2 examples, 0 failures
```

## Deep Dive

Testaus on kehittynyt vuosien varrella osaksi ohjelmistokehityksen perusta. Testivetoisen kehityksen (Test-Driven Development, TDD) metodit ja käyttäytyminen määrittävä kehitys (Behavior-Driven Development, BDD) ovat esimerkkejä lähestymistavoista. Vaihtoehtoisia työkaluja Ruby-testaukseen ovat MiniTest ja Test::Unit. Tärkeää on tietää, milloin käyttää stubbia, mockia tai faketta.

## See Also

- RSpec ohjekirja: https://relishapp.com/rspec
- Ruby testausframeworkien vertailu: https://www.ruby-toolbox.com/categories/testing_frameworks
- TDD:n opas: http://www.tddfellow.com/blog/2016/05/31/what-is-tdd-and-why-should-you-use-it/
