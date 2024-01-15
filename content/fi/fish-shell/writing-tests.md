---
title:                "Testiaineistojen kirjoittaminen"
html_title:           "Fish Shell: Testiaineistojen kirjoittaminen"
simple_title:         "Testiaineistojen kirjoittaminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Kuvittele, että kirjoitat koodia ja kaikki tuntuu sujuvan hyvin. Sitten joudut tekemisiin erilaisten muutosten ja päivitysten kanssa, ja huomaat, että koodisi ei enää toimikaan odotetulla tavalla. Testien kirjoittaminen voi auttaa välttämään tällaiset yllätykset ja säästää aikaa ja päänsärkyä.

## Kuinka

```Fish Shell (nykyinen versio)``` mahdollistaa testien kirjoittamisen omassa erityisessä ```test``` -kansiossa. Tämä kansio sisältää testitiedostoja, jotka ovat nimetty ```filename.test.fish```. Voit testata yksittäisiä funktioita tai koko skriptejä. Testit suoritetaan komennolla ```fish --test tests```. 

## Syväsukellus

Testien kirjoittaminen Fish Shellille tapahtuu käyttämällä sisäänrakennettuja ```test``` -komennolla. Tässä muutamia esimerkkejä, miten voit testata koodiasi:

1. Testaa funktiota nimeltä ```greet```, joka tulostaa tervehdyksen:
```
function greet
  echo "Hello World"
end

test "greet tulostaa tervehdyksen" 
  greet | grep -q "Hello World"
end
```
2. Voit myös testata koko skriptiä, kunhan varmistat, että ```test``` -komennossa annat skriptin nimen ja tiedoston polun:
```
test "Kirjoitettu skripti toimii odotetulla tavalla" 
  fish ~/Documents/scripts/validate.fish
end
```

## Katso myös

- [Fish Shellin virallinen dokumentaatio](https://fishshell.com/docs/current/cmds/test.html)
- [A detailed guide to writing tests in Fish Shell](https://hackernoon.com/a-detailed-guide-to-writing-tests-in-fish-shell-8ul3z3b9)