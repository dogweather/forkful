---
title:                "Testien kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? - Mikä ja Miksi?
Testaus auttaa varmistamaan koodin toimivuuden. Ohjelmoijat tekevät sitä vikoja estääkseen ja luotettavuutta parantaakseen.

## How to: - Kuinka tehdään:
Fish Shellissä voit kirjoittaa testitiedostoja `.fish` -päätteellä. Ohjelmoi testit tavallisena skriptinä, oleta toiminta ja tarkista tulokset.

```Fish Shell
function test_greeting
  set actual (echo "Moi $argv")
  set expected "Moi Fish"
  if test "$actual" = "$expected"
    echo "Test passed: greeting is correct"
  else
    echo "Test failed: expected $expected, got $actual"
  end
end

test_greeting Fish
```

Tulostaisi:

```
Test passed: greeting is correct
```

## Deep Dive - Syväsukellus:
Testauksen juuret ovat ohjelmistotuotannossa. Fishin omat työkalut ovat rajalliset, mutta ulkoisia työkaluja kuten `fishtape` voi käyttää. Testien toteutus voi olla käsin kirjoitettuja vertailuja tai kehittyneempiä testirunkoja.

## See Also - Katso myös:
- Fish Shell dokumentaatio: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- `fishtape`, Fishin testityökalu: [https://github.com/jorgebucaran/fishtape](https://github.com/jorgebucaran/fishtape)
- Ohjelmistotestauksen perusteet: [https://martinfowler.com/testing/](https://martinfowler.com/testing/)
