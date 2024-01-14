---
title:                "Fish Shell: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittamalla testejä varmistat, että koodisi toimii kuten odotat ja vähentää mahdollisia virheitä tulevaisuudessa.

## Kuinka

```Fish Shell``` -käyttö esimerkkien ja tulosteen kanssa auttaa sinua ymmärtämään, miten testien kirjoittaminen toimii Fish Shell -ohjelmointikielessä.

1. Aseta ympäristömuuttujat
```
set -x TEST_VAR "Hello World"
set -x ANOTHER_VAR "Bye Bye"
```

2. Luo testitiedosto
```
@test "TEST_VAR on oikea arvo" 
set ACTUAL_VAL $TEST_VAR
set EXPECTED_VAL "Hello World"
[ $ACTUAL_VAL != $EXPECTED_VAL ] && echo "FAIL: Odotettiin $EXPECTED_VAL, saatiin $ACTUAL_VAL"  
done
```

3. Suorita testitiedosto
```
fish testi.sh
```

Output:
```
🐟 testi.sh
TEST_VAR on oikea arvo: Passed
```

## Syvempää tarkastelua

Testien kirjoittaminen on tärkeä osa koodausprosessia, jotta voidaan varmistaa luotettava ja toimiva koodi. Hyvät testit antavat myös mahdollisuuden tehdä muutoksia koodiin turvallisesti ja helposti.

## Katso myös

[Fish Shell Tutorial](https://fishshell.com/docs/current/tutorial.html)
[Fish Shell -dokumentaatio](https://fishshell.com/docs/current/index.html)