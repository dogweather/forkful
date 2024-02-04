---
title:                "Testien kirjoittaminen"
date:                  2024-02-03T19:29:41.161254-07:00
model:                 gpt-4-0125-preview
simple_title:         "Testien kirjoittaminen"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?
Testien kirjoittaminen Bashissa käsittää testitapausten skriptaamisen Bash-skriptien toiminnallisuuden varmistamiseksi. Ohjelmoijat suorittavat testejä varmistaakseen, että heidän skriptinsä toimivat odotetusti erilaisissa olosuhteissa, ja nappaamaan virheet ja bugit ennen käyttöönottoa.

## Kuinka:
Bashissa ei ole sisäänrakennettua testauskehystä, mutta voit kirjoittaa yksinkertaisia testifunktioita. Monimutkaisempia testejä varten suosittuja ovat kolmansien osapuolien työkalut kuten `bats-core`.

### Perustestiesimerkki Puhtaalla Bashilla:
```bash
function test_example_function {
  result=$(your_function 'test_input')
  expected_output="expected_output"
  
  if [[ "$result" == "$expected_output" ]]; then
    echo "Testi läpäisty."
    return 0
  else
    echo "Testi epäonnistui. Odotettiin '$expected_output', saatiin '$result'"
    return 1
  fi
}

# Testifunktion kutsuminen
test_example_function
```
Esimerkkituloste:
```
Testi läpäisty.
```

### `bats-core`-työkalun käyttö testauksessa:
Asenna ensin `bats-core`. Tämä voidaan yleensä tehdä paketinhallintasi kautta tai kloonaamalla sen repository.

Kirjoita sitten testisi erillisiin `.bats`-tiedostoihin.

```bash
# Tiedosto: example_function.bats

#!/usr/bin/env bats

@test "testaa esimerkkifunktiota" {
  result="$(your_function 'test_input')"
  expected_output="expected_output"
  
  [ "$result" == "$expected_output" ]
}
```
Ajaaksesi testisi, suorita yksinkertaisesti `.bats`-tiedosto:
```bash
bats example_function.bats
```
Esimerkkituloste:
```
 ✓ testaa esimerkkifunktiota

1 testi, 0 epäonnistumista
```

Tämä lähestymistapa mahdollistaa testauksen helposti integroinnin kehitystyönkulkuusi, varmistamalla Bash-skriptiesi luotettavuuden ja vakauden.
