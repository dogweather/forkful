---
title:                "Fish Shell: HTML-analysointi"
simple_title:         "HTML-analysointi"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi

Monissa web-kehitysprojekteissa on tarve käsitellä HTML-tiedostoja ja saada siitä tarvittavat tiedot esimerkiksi sivujen sisällön, linkkien tai kuvien osalta. Fish Shellin avulla tämä voidaan tehdä helposti ja nopeasti ilman erillistä web-kehitystyökaluja.

## Miten

Fish Shell tarjoaa useita käteviä toimintoja HTML-tiedostojen lukemiseen ja parsimiseen. Aloittaaksesi, sinun on asennettava Fish Shell ja lisättävä HTML-parsing-kirjasto kuten "html-xml-utils" käyttöösi.

```Fish Shell 
# Asenna Fish Shell
sudo apt-get install fish 

# Asenna html-xml-utils-kirjasto
sudo apt-get install html-xml-utils 
```

Nyt voit käyttää Fish Shellin komentoja kuten "hxselect" ja "hxpipe" HTML-tiedostojen käsittelyyn ja tiedon eristämiseen. Esimerkiksi jos haluat hakea tietyn tyylin sisältävät elementit HTML-tiedostosta ja tulostaa ne, käytä seuraavaa komentoa:

```Fish Shell 
# Hae kaikki <p> tagit HTML-tiedostosta ja tulosta ne
hxselect p testi.html 
```

Tulosteena näet kaikki <p> tagien sisällöt testi.html-tiedostossa. Voit myös yhdistää useita komentoja käyttämällä "hxpipe" komentoa. Esimerkiksi, jos haluat käsitellä tiettyjä linkkejä ja tulostaa niiden URL-osoitteet, voit käyttää seuraavaa komentoa:

```Fish Shell 
# Hae kaikki <a> tagit sivulta ja tulosta niiden "href" attribuutit
hxselect a | hxpipe -c 'xml:attr(href)' testi.html 
```

Tämä käsky haastaa kaikki "a" tagit testi.html-tiedostosta ja käyttää sitten "hxpipe" komentoa eristääksesi jokaisen linkin "href" attribuutin ja tulostaa ne.

## Syvempi sukellus

Fish Shell mahdollistaa myös HTML-tiedostojen käsittelyn yhdistämällä sen muihin komentorivin työkaluihin, kuten grep ja sed. Voit myös käyttää awk- ja perl-komentoja suurempien ja monimutkaisempien tiedostojen käsittelyyn.

Lisäksi Fish Shellin daten virtaamispohjainen luonne tekee tiedostojen käsittelystä nopeampaa ja tehokkaampaa. Voit käyttää "jq" komentoa JSON-tiedostojen parsimiseen ja "curl" komentoa tiedostojen lataamiseen suoraan verkosta.

## Katso myös

- [Fish Shell opas (suomeksi)](https://fishshell.com/docs/current/tutorial.html)
- [Fish Shell virallinen dokumentaatio (englanniksi)](https://fishshell.com/docs/current/index.html)
- [html-xml-utils-kirjaston dokumentaatio (englanniksi)](https://neilb.me/2005/04/26/html-xml-utils/)
- [Onnistuneen Fish Shell työnkulun rakentaminen HTMListen-kirjaston avulla (englanniksi)](https://melanie-richards.com/blog/how-to-build-a-useful-fish-shell-workflow-with-html-xml-utils/)