---
title:                "Tekstin etsiminen ja vaihtaminen"
html_title:           "Fish Shell: Tekstin etsiminen ja vaihtaminen"
simple_title:         "Tekstin etsiminen ja vaihtaminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?

Hakeminen ja tekstin korvaaminen ovat kaksi yleistä toimintoa, joita ohjelmoijat tekevät koodinsa kanssa. Hakeminen tarkoittaa tietyn tekstin löytämistä tiedostosta tai koodista, kun taas korvaaminen tarkoittaa kyseisen tekstin vaihtamista johonkin toiseen. Nämä toiminnot ovat tärkeitä, sillä ne auttavat meitä muokkaamaan ja päivittämään koodiamme nopeammin ja tehokkaammin.

# Miten tehdä:

```Fish Shell``` tarjoaa useita tapoja hakemisen ja korvaamisen toteuttamiseen. Tässä muutamia esimerkkejä:

1. Hakeminen: Voit käyttää ```grep``` komentoa hakemaan tiettyä tekstiä tiedostosta tai koodista. Esimerkiksi ```grep "haku" tiedosto.txt``` hakee kaikki tiedoston sisältämät rivit, jotka sisältävät sanan "haku".

2. Korvaaminen: Voit käyttää ```sed``` komentoa vaihtamaan tietyn tekstin toiseen tiedostossa tai koodissa. Esimerkiksi ```sed -i 's/vanha/uusi/' tiedosto.txt``` korvaa kaikki esiintymät sanasta "vanha" sanalla "uusi".

3. Moniriviset korvaukset: Voit käyttää ```perl``` komentoa hakemaan ja korvaamaan monirivisiä lohkoja tiedostosta tai koodista. Esimerkiksi ```perl -0777 -pi -e 's/vanha/uusi/g' tiedosto.txt``` korvaa kaikki esiintymät sanasta "vanha" sanalla "uusi" koko tiedostossa.

# Syväsukellus:

Hakemista ja korvaamista on käytetty jo pitkään ohjelmointimaailmassa helpottamaan koodin muokkausta ja päivittämistä. Yleisimpiä vaihtoehtoja ```Fish Shell```n lisäksi ovat Bash, Zsh ja PowerShell. Nämä eri vaihtoehdot tarjoavat samankaltaisia toimintoja, mutta eroavat esimerkiksi syntaksin ja käytön suhteen.

Hakemista ja korvaamista voidaan myös toteuttaa koodin sisällä muilla tavoilla, kuten Pythonin ```re``` kirjastolla. Lisäksi on olemassa useita ohjelmia ja sovelluksia, jotka on suunniteltu erityisesti tekstien hakemiseen ja korvaamiseen.

Jos haluat tietää enemmän hakemisesta ja korvaamisesta ```Fish Shell```n ulkopuolella, voit tutustua artikkeleihin ja dokumentaatioihin, jotka käsittelevät tätä aihepiiriä.

# Katso myös:

- [Fish Shell käyttöohjeet](https://fishshell.com/docs/current/index.html)
- [GREP-komennon opas](https://www.lifewire.com/uses-of-linux-command-grep-4058969)
- [SED-komennon opas](https://www.grymoire.com/Unix/Sed.html)
- [PERL-komennon opas](https://www.programiz.com/perl-programming/regex)