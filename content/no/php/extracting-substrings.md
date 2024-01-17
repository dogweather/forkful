---
title:                "Ekstrahering av delstrenger"
html_title:           "PHP: Ekstrahering av delstrenger"
simple_title:         "Ekstrahering av delstrenger"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/extracting-substrings.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?

Ekstrahering av substrings er prosessen med å isolere en del av en tekststreng basert på et gitt mønster eller en bestemt posisjon. Dette er nyttig for å manipulere og behandle data, og for å få tilgang til spesifikke deler av en lengre tekst. Programmere bruker substrings for å effektivt håndtere tekst og få tilgang til nøyaktig den informasjonen de trenger.

Hvordan:

For å ekstrahere en substring i PHP bruker vi funksjonene `substr` eller `mb_substr`. Funksjonen `substr` tar inn tre parametere: tekststrengen vi vil ekstrahere fra, startposisjonen til substringen og lengden på substringen. Funksjonen `mb_substr` er nyttig når vi håndterer flerspråklige tekststrenger, og tar inn samme parametere som `substr`.

```PHP
$text = "Dette er en tekststreng";
// Ekstrahere "er en"
echo substr($text, 6, 5);
// Output: er en

// Ekstrahere "tekst" ved å bruke negativ startposisjon
echo substr($text, -13, 5);
// Output: tekst

// Ekstrahere "tekst" ved å bruke mb_substr
echo mb_substr($text, 6, 5);
// Output: tekst
```

Dypdykk:

Ekstrahering av substrings har vært en viktig del av tekstbehandling i programmering siden begynnelsen. Tidligere var det vanlig å bruke funksjoner som `substr` og `strpos` (for å finne posisjonen til en bestemt tekst i en streng) for å behandle tekst, men med introduksjonen av regulære uttrykk har det blitt enklere og mer effektivt å ekstrahere substrings som passer til et spesifikt mønster.

Alternativene til å bruke substrings i PHP er å bruke eksterne tekstbehandlingsbiblioteker som `mbstring` og `iconv`. Disse kan gi mer fleksibilitet og støtte for ulike tegnsett, men kan også være mer komplekse å bruke.

Et viktig poeng å merke seg er at substrings i PHP gjelder for 8-bits tegnsett. Hvis du håndterer flerspråklige tekststrenger som inneholder tegn fra ulike tegnsett, er det anbefalt å bruke funksjoner som `mb_substr` for å unngå feil ved ekstrahering av substrings.

Se også:

- PHP manual for `substr`: https://www.php.net/manual/en/function.substr.php
- PHP manual for `mb_substr`: https://www.php.net/manual/en/function.mb-substr.php
- PHP text processing tutorials: https://www.php.net/manual/en/ref.strings.php