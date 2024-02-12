---
title:                "Generering av tilfeldige tall"
date:                  2024-01-27T20:34:57.089310-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generering av tilfeldige tall"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Det å generere tilfeldige tall i PHP handler om å produsere uforutsigbare verdier innenfor et spesifisert område, noe som er essensielt for oppgaver som å skape unike bruker-IDer, generere passord, eller for bruk i simuleringer og spill. Programmerere stoler på tilfeldighet for å tilføre uforutsigbarhet og variasjon i applikasjonene sine, noe som gjør prosesser som testing eller brukeropplevelser mer robuste og engasjerende.

## Hvordan:

PHP tilbyr flere funksjoner for å generere tilfeldige tall, men de mest brukte er `rand()`, `mt_rand()`, og for kryptografiske formål, `random_int()`.

For å generere et enkelt tilfeldig tall mellom 0 og getrandmax() (den største mulige verdien returnert av `rand()`), kan du bruke:

```PHP
echo rand();
```

For et mer spesifikt område, som mellom 1 og 100:

```PHP
echo rand(1, 100);
```

Imidlertid er `mt_rand()` et bedre valg for fart og tilfeldighet:

```PHP
echo mt_rand(1, 100);
```

Resultatet for begge kan være noe mellom 1 og 100, avhengig av tilfeldiggjøringen, f.eks., `42`.

For kryptografiske eller sikkerhetskontekster, der uforutsigbarhet er avgjørende, er `random_int()` det foretrukne valget ettersom den genererer kryptografisk sikre pseudo-tilfeldige heltall:

```PHP
echo random_int(1, 100);
```

Igjen er resultatet et tilfeldig tall mellom 1 og 100, som `84`, men med en sterkere garanti for tilfeldighet.

## Dypdykk

`rand()`-funksjonen har vært til stede i PHP siden de tidlige versjonene, og fungerte som den opprinnelige metoden for å generere tilfeldige tall. Imidlertid er ikke dette det beste valget for applikasjoner som krever en høy grad av tilfeldighet på grunn av dens relativt forutsigbare algoritme.

`mt_rand()`, introdusert i PHP 4, er basert på Mersenne Twister-algoritmen - langt overlegen i form av hastighet og den tilfeldigheten den kan generere sammenlignet med `rand()`. Den ble raskt det foretrukne alternativet for de fleste ikke-kryptografiske behov.

For sikkerhetskritiske applikasjoner, ble `random_int()` introdusert i PHP 7 for å generere kryptografisk sikre pseudo-tilfeldige heltall ved å bruke tilfeldige bytes fra systemets tilfeldige tallgenerator. Det er vesentlig sikrere enn både `rand()` og `mt_rand()`, noe som gjør den til det beste valget for å generere token, nøkler eller andre elementer hvor forutsigbarhet kan føre til sikkerhetsrisikoer.

Til tross for disse forbedringene, er det avgjørende å velge riktig funksjon basert på applikasjonens kontekst. For generell bruk, holder `mt_rand()`, men for alt som kan bli rettet mot eller utnyttet, er `random_int()` veien å gå, og tilbyr både tilfeldighet og sikkerhet.