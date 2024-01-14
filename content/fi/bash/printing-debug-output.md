---
title:                "Bash: Virheenjäljitystulostuksen tulostaminen"
programming_language: "Bash"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Koodissa törmää usein ongelmiin, jotka vaativat tarkempaa tarkkailua. Debuggaamiseen käytetään usein print-lauseita, joiden avulla voidaan tulostaa tietoa ja seurata koodin suoritusta. Tämä on tärkeää, jotta voidaan havaita missä kohtaa koodi ehkä ei toimi oletetulla tavalla. Siksi debug-tulostus on tärkeä osa Bash-ohjelmointia.

## Kuinka

Bash-koodissa voit tulostaa debug-tietoja käyttämällä komentoa "echo". Esimerkiksi:

```Bash
echo "Debug-tietoa tässä"
```

Tulostuksena näet "Debug-tietoa tässä". Voit myös tulostaa muuttujien arvoja laittamalla ne lainausmerkkien sisään:

```Bash
muuttuja="Tämä on debug-tietoa"
echo "Muuttujan arvo on: $muuttuja"
```

Tulostuksena näet "Muuttujan arvo on: Tämä on debug-tietoa".

## Syväsukellus

Debug-tulostuksen voi tehdä myös käyttämällä "-x" -valitsinta. Tämä vaihtoehto tulostaa jokaisen komennon ennen sen suorittamista ja sen jälkeen. Voit käyttää sitä esimerkiksi ajamalla skriptin seuraavasti:

```Bash
bash -x skripti.sh
```

Tällöin näet jokaisen komennon, jota skripti suorittaa, ja sen jälkeisen tulosteen.

Voit myös käyttää komentoa "set -x" asettaaksesi koodiin tietyn kohdan, jossa debug-tulostus alkaa ja "set +x" asettaaksesi kohdan, jossa se loppuu. Tämä voi olla hyödyllistä, jos haluat tarkastella vain tiettyä osaa koodista.

## Katso myös

- [Bashin virallinen manuaali](https://www.gnu.org/software/bash/manual/bash.html)
- [Debug-tulostuksen käyttämisen parhaat käytännöt](https://www.linuxjournal.com/content/bash-debugging-made-easy)
- [Debug-tulostuksen käyttöönotto Bash-skripteissä](https://www.tecmint.com/debug-shell-scripts-linux/)