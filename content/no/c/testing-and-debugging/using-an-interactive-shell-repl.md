---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:12.657813-07:00
description: "Hvordan: \xC5 engasjere seg med en C REPL, kan du kanskje ikke finne\
  \ en like enkel vei som i spr\xE5k som Python eller JavaScript. Men, verkt\xF8y\
  \ som `Cling`, en\u2026"
lastmod: '2024-03-13T22:44:41.273707-06:00'
model: gpt-4-0125-preview
summary: "\xC5 engasjere seg med en C REPL, kan du kanskje ikke finne en like enkel\
  \ vei som i spr\xE5k som Python eller JavaScript."
title: Bruk av interaktiv shell (REPL)
weight: 34
---

## Hvordan:
Å engasjere seg med en C REPL, kan du kanskje ikke finne en like enkel vei som i språk som Python eller JavaScript. Men, verktøy som `Cling`, en C/C++ tolker basert på Clang og LLVM teknologi, gjør det mulig. Her er hvordan du kommer i gang:

1. **Installer Cling**: Avhengig av operativsystemet ditt, kan du finne Cling i pakkebehandleren din eller trenge å bygge fra kilde. For eksempel, på Ubuntu, kan det være så enkelt som `sudo apt-get install cling`.

2. **Starte Cling**: Åpne terminalen din og skriv inn `cling` for å starte det interaktive skallet.

```bash
$ cling
```

3. **Skrive Kode**: Nå kan du skrive C-kode direkte inn i skallet og se umiddelbare resultater. Her er et enkelt eksempel:

```c
[cling]$ #include <stdio.h>
[cling]$ printf("Hei, REPL verden!\n");
Hei, REPL verden!
```

4. **Eksempel med Variabler og Operasjoner**: Eksperimenter med variabler og se øyeblikkelig tilbakemelding.

```c
[cling]$ int a = 5;
[cling]$ int b = 3;
[cling]$ printf("%d + %d = %d\n", a, b, a+b);
5 + 3 = 8
```

5. **Inkludere Biblioteker**: Cling lar deg inkludere biblioteker på fly, og lar deg dermed aktivere et bredt spekter av C-funksjonaliteter.

```c
[cling]$ #include <math.h>
[cling]$ printf("Kvadratroten av %f er %f\n", 4.0, sqrt(4.0));
Kvadratroten av 4.000000 er 2.000000
```

## Dypdykk:
Utviklingen av REPL-miljøer går tilbake til Lisp på 1960-tallet, designet for å støtte interaktiv kodeevaluering. Men, den statiske og kompilerte naturen til C utgjorde utfordringer for å realisere lignende umiddelbarhet i justeringer av kodeutførelse. Utviklingen av Cling og andre C/C++ tolkere markerer betydelige fremskritt mot å integrere dynamisk evaluering i statisk typede språk.

Fremhevet er det at ved bruk av en tolker som Cling, kan det hende at det ikke helt etterligner oppførselen til kompilert C-kode på grunn av forskjeller i optimalisering og utførelse. Også, selv om det er svært verdifullt for utdanning, rask prototyping, og feilsøking, kan REPL-er for C noen ganger være tregere og mindre praktiske for utvikling av kode på produksjonsnivå sammenlignet med tradisjonelle kompilere-kjøre-feilsøke sykluser.

Alternativer for interaktiv C-programmering inkluderer å skrive små, selvstendige programmer og bruke robuste IDE-er med integrerte feilsøkingsverktøy, som kan tilby mer kontroll og innsikt i utførelse, om enn med mindre umiddelbarhet. Til tross for disse alternativene, utgjør utviklingen av REPL-miljøer i C en spennende utvidelse av språkets allsidighet og omfavner kravene i den moderne æraen for fleksibilitet og hastighet i utviklingssykluser.
