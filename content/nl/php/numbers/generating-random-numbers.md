---
aliases:
- /nl/php/generating-random-numbers/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:18.747022-07:00
description: "Het genereren van willekeurige getallen in PHP gaat over het produceren\
  \ van onvoorspelbare waarden binnen een gespecificeerd bereik, wat essentieel is\u2026"
lastmod: 2024-02-18 23:09:01.940419
model: gpt-4-0125-preview
summary: "Het genereren van willekeurige getallen in PHP gaat over het produceren\
  \ van onvoorspelbare waarden binnen een gespecificeerd bereik, wat essentieel is\u2026"
title: Willekeurige getallen genereren
---

{{< edit_this_page >}}

## Wat & Waarom?

Het genereren van willekeurige getallen in PHP gaat over het produceren van onvoorspelbare waarden binnen een gespecificeerd bereik, wat essentieel is voor taken zoals het creëren van unieke gebruikers-ID's, het genereren van wachtwoorden, of voor gebruik in simulaties en spellen. Programmeurs vertrouwen op willekeur om onvoorspelbaarheid en variabiliteit in hun applicaties te brengen, waardoor processen zoals testen of gebruikerservaringen robuuster en boeiender worden.

## Hoe te:

PHP biedt verschillende functies voor het genereren van willekeurige getallen, maar de meest gebruikte zijn `rand()`, `mt_rand()`, en voor cryptografische doeleinden `random_int()`.

Om een eenvoudig willekeurig getal tussen 0 en getrandmax() (de grootst mogelijke waarde geretourneerd door `rand()`) te genereren, kun je gebruiken:

```PHP
echo rand();
```

Voor een specifieker bereik, zoals tussen 1 en 100:

```PHP
echo rand(1, 100);
```

Echter, `mt_rand()` is een betere keuze voor snelheid en willekeur:

```PHP
echo mt_rand(1, 100);
```

De uitvoer voor beiden kan alles tussen 1 en 100 zijn, afhankelijk van de randomisatie, bijv., `42`.

Voor cryptografische of beveiligingscontexten, waar onvoorspelbaarheid cruciaal is, is `random_int()` de voorkeurskeuze omdat het cryptografisch veilige pseudo-willekeurige getallen genereert:

```PHP
echo random_int(1, 100);
```

Opnieuw is de uitvoer een willekeurig getal tussen 1 en 100, zoals `84`, maar met een sterkere garantie van willekeur.

## Diepgaand

De `rand()` functie is al sinds de eerste versies van PHP aanwezig en diende als de initiële benadering voor het genereren van willekeurige getallen. Het is echter niet de beste keuze voor toepassingen die een hoge mate van willekeur vereisen vanwege zijn relatief voorspelbare algoritme.

`mt_rand()`, geïntroduceerd in PHP 4, is gebaseerd op het Mersenne Twister-algoritme - ver superieur in termen van snelheid en de willekeur die het kan genereren in vergelijking met `rand()`. Het werd snel de voorkeursoptie voor de meeste niet-cryptografische behoeften.

Voor beveiligingsgevoelige applicaties werd `random_int()` in PHP 7 geïntroduceerd om cryptografisch veilige pseudo-willekeurige gehele getallen te genereren met behulp van willekeurige bytes uit de systeembrede random number generator van het systeem. Het is aanzienlijk veiliger dan `rand()` of `mt_rand()`, waardoor het de beste keuze is voor het genereren van tokens, sleutels of andere elementen waar voorspelbaarheid kan leiden tot beveiligingskwetsbaarheden.

Ondanks deze verbeteringen, is het cruciaal om de juiste functie te kiezen op basis van de context van de applicatie. Voor algemeen gebruik volstaat `mt_rand()`, maar voor alles dat kan worden gericht of geëxploiteerd, is `random_int()` de weg te gaan, wat zowel willekeur als beveiliging biedt.
