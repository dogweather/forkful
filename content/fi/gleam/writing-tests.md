---
title:                "Testien kirjoittaminen"
html_title:           "Arduino: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Testaus tarkoittaa koodillesi haasteiden asettamista: toimiiko se odotetusti? Ohjelmoijat testaavat välttääkseen bugeja ja säästääkseen aikaa tulevaisuudessa. Turvallinen ja luotettava koodi syntyy testien avulla.

## How to: (Kuinka tehdä:)
Gleamissa testien kirjoittaminen on mutkatonta. Esimerkkikoodi alla:

```gleam
import gleam/should
import my_module

pub fn add_test() {
  should.equal(my_module.add(1, 2), 3)
}

pub fn subtract_test() {
  should.equal(my_module.subtract(5, 3), 2)
}
```

Suorita testit komennolla `gleam test`. Jos kaikki toimii, näet jotain tällaista:

```
running 2 tests
test my_module.add_test ... ok
test my_module.subtract_test ... ok

test result: ok. 2 passed; 0 failed; 0 ignored
```

## Deep Dive (Syväsukellus)
Gleam on nuori kieli, joka alkoi vuonna 2018. Testien kirjoittamiselle on muitakin tapoja, kuten ETest tai Proper, mutta Gleam suosii omia yksikkötestaustyökalujaan. Suorituskyky ja helppous tekevät Gleam-testeistä ihanteellisen vaihtoehdon.

## See Also (Katso myös)
- Gleamin viralliset dokumentaatiot testauksesta: https://gleam.run/book/tour/testing.html
- Esimerkkirepo täynnä Gleam-koodia: https://github.com/gleam-lang/example
- Keskustelua Gleamin testauskehyksistä: https://github.com/gleam-lang/suggestions/discussions
