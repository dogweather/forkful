---
title:                "Vianmääritystulostus"
html_title:           "Bash: Vianmääritystulostus"
simple_title:         "Vianmääritystulostus"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Tiedätkö sen tunteen, kun koodia on kirjoitettu tuntikausia ja se ei toimi? Se voi olla todella turhauttavaa. Tästä syystä on tärkeää lisätä debug-tulostusta koodiisi. Tämä auttaa sinua ymmärtämään, mitä koodisi todella tekee ja miksi se ei toimi odotetulla tavalla. Se on kuin katsoa koodisi lasin läpi ja selvittää, mikä sen toimintaa hidastaa.

## Kuinka

Aloitetaanpa perusteilla. Bashilla on sisäänrakennettu komento "echo", joka tulostaa halutun tekstin tai arvon. Voit käyttää tätä komentoa debug-tulostukseen lisäämällä sen haluamaasi kohtaan koodissasi.

```Bash 
echo "Sisäänkirjautuminen suoritettu onnistuneesti"
```

Tämä tulostaisi tekstin "Sisäänkirjautuminen suoritettu onnistuneesti" terminaaliisi. Tämä auttaa sinua selvittämään, mikä osa koodiasi on juuri suoritettu.

Voit myös tulostaa muuttujien arvoja käyttämällä muuttujien nimiä yhdessä "echo" -komennon kanssa. Tämä on erittäin hyödyllistä, jos epäilet, että jokin muuttuja ei ole oikein määritetty.

```Bash
Nimi="Matti"
echo "Käyttäjän nimi on: $Nimi"
```

Tämä tulostaisi "Käyttäjän nimi on: Matti". Näin voit varmistaa, että muuttuja on oikein asetettu ja voit jäljittää ongelmia helpommin.

## Syväsukellus

On myös muita tapoja lisätä debug-tulostusta bash-koodiin. Voit esimerkiksi käyttää "set -x" -komennon avulla aktivoidaksesi koko scriptin debug-tulostuksen. Tämä tulostaa kaikki komentoriviltä suoritetut komentosi, joten voit seurata koodin suorittamista vaihe vaiheelta.

Voit myös tallentaa debug-tulostuksen tiedostoon lisäämällä "> debug.log" -tarjaimen komentosi perään. Tämä tallentaa kaikki tulostetut asiat tiedostoon, jotta voit tarkastella niitä myöhemmin.

## Katso myös

- [Bashin virallinen dokumentaatio](https://www.gnu.org/software/bash/manual/bash.html)
- [Debug-tulostuksen lisääminen Shell-skripteihin](https://www.linuxjournal.com/content/add-debugging-your-shell-scripts-bash-x-trace-and-shell-debugger)
- [Debugging in Bash](https://www.baeldung.com/linux/bash-debugging)