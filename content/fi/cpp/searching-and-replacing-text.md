---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "C++: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Ehkä olet törmännyt ongelmaan, jossa sinun pitää muuttaa suuri määrä tekstiä samassa tiedostossa tai projektissa. Onneksi ohjelmoijilla on käytössään tehokas työkalu nimeltä "tekstin etsiminen ja korvaaminen" (engl. "search and replace"), jonka avulla tämä työ sujuu nopeasti ja vaivattomasti. Etsiminen ja korvaaminen mahdollistaa tekstin löytämisen ja sen korvaamisen uudella tekstillä hyvin nopeasti, mikä säästää aikaa ja vaivaa. 

## Miten:
Etsimisen ja korvaamisen käyttö on yksinkertaista ohjelmoijille. Katso esimerkki alla olevasta koodinpätkästä, joka etsii ja korvaa kaikki "kissa" sanat "koira" sanoilla.

```C++
std::string teksti = "Näin kissa loikkaa pöydän yli ja nappaa lelunsa.";
teksti.replace("kissa", "koira");
std::cout << teksti << std::endl;
```

Tulostus: "Näin koira loikkaa pöydän yli ja nappaa lelunsa."

Voit myös etsiä ja korvata tekstin tietyn alueen sisältä. Katso esimerkki alla, jossa etsitään ja korvataan vain ensimmäinen "kissa" sana.

```C++
std::string teksti = "Kissa loikkaa pöydän yli ja kissa nappaa lelunsa.";
teksti.replace(teksti.find("kissa"), 5, "koira");
std::cout << teksti << std::endl;
```

Tulostus: "Koira loikkaa pöydän yli ja kissa nappaa lelunsa."

## Deep Dive:
Etsimistä ja korvaamista on käytetty ohjelmoinnissa jo vuosikymmenien ajan. Alun perin se oli käsintehtävä, jossa ohjelmoijat etsivät koodistaan tiettyjä sanoja tai lausekkeita ja korvasivat ne uusilla. Nykyään ohjelmointiympäristöt tarjoavat mahdollisuuden automatisoida tämä prosessi, mikä säästää paljon aikaa ja vaivaa.

On myös olemassa muita työkaluja, kuten "bulk replace", jotka voivat olla hyödyllisiä, jos haluat etsiä ja korvata tekstiä useista eri tiedostoista tai projekteista.

Tekstin etsimisen ja korvaamisen toteuttaminen tapahtuu monella eri tavalla, ja se riippuu ohjelmointiympäristöstä ja käytetyistä työkaluista. Esimerkiksi joidenkin ohjelmointikielten kirjastoissa on valmiina toiminto tekstiin etsimistä ja korvaamista varten.

## See Also:
Lyhyesti, tämä artikkeli antaa sinulle yleiskuvan siitä, mitä tekstien etsiminen ja korvaaminen ovat ja kuinka niitä käytetään ohjelmoinnissa. Suosittelemme myös tutustumaan seuraaviin lähteisiin, jotka voivat auttaa sinua lisää:

- [Regular expressions tutorial](https://regexone.com/)
- [Official C++ documentation](https://en.cppreference.com/w/cpp/string/basic_string/replace)
- [Bulk replace tool](https://www.bulkrenameutility.co.uk/), joka tarjoaa enemmän vaihtoehtoja kuin useimmat tekstieditorit