---
title:                "Aloittaminen uuden projektin"
html_title:           "C: Aloittaminen uuden projektin"
simple_title:         "Aloittaminen uuden projektin"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Alkamassa Uusi Projekti Gleam-ohjelmointikielellä 

## Mikä & Miksi?

Aloitamme uuden projektin, kun haluamme luoda jotain uutta. Koodaajat tekevät tämän jatkuvasti, koska se on tapa oppia, kokeilla uusia ideoita ja löytää ratkaisuja ongelmiin.

## Miten tehdä:

Luodaan uusi Gleam-projekti nimeltä "hello_gleam" käyttämällä Rebar3: n käynnistyskomentoa. Koodimme näyttää tältä:

```Gleam
$ rebar3 new gleam_lib hello_gleam
$ cd hello_gleam
$ rebar3 eunit
```

Suorittamalla yllä olevat komennot luo uuden projektihakemiston "hello_gleam", jossa voit alkaa kehittää Gleam-ohjelmistoasi.

## Syvä Sukellus:

Gleam on suhteellisen uusi kieli, joka on luotu tarjoamaan turvallisempi, tehokkaampi ja nautittavampi tapa kirjoittaa Erlang/OTP-sovelluksia. Aikaisemmin Erlang/OTP-projekteja varten on luotu useita projektiluomistyökaluja, mukaan lukien 'rebar', 'relx' ja 'erlang.mk', mutta 'Rebar3' on nykyään yleisin ja suositelluin. 

'hello_gleam'-esimerkki näyttää, kuinka voit alkaa kehittää Gleam-projekti 'Rebar3':lla. Kun oivalletaan 'Rebar3':n rakenteiden luoma rakenne, voit tästä lähteä toteuttamaan monimutkaisempia sovelluksia ja projekteja.

## Katso myös: 

- [Gleam-koti](https://gleam.run/)
- [Gleam GitHub](https://github.com/gleam-lang/gleam)
- [Rebar3 opas](https://www.rebar3.org/docs/getting-started)