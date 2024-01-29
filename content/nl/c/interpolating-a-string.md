---
title:                "Een string interpoleren"
date:                  2024-01-28T22:02:07.599209-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string interpoleren"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Stringinterpolatie maakt het mogelijk om variabelen rechtstreeks in strings in te voegen. Het is handig om dynamisch tekst te creëren, zoals gepersonaliseerde berichten of geformatteerde gegevens.

## Hoe doe je het:

C heeft geen ingebouwde stringinterpolatie, maar we improviseren met `sprintf` of `snprintf`. Zo werkt het:

```c
#include <stdio.h>

int main() {
    char name[] = "Alex";
    int age = 29;
    char message[50];

    snprintf(message, sizeof(message), "Hoi, ik ben %s, en ik ben %d jaar oud.", name, age);
    printf("%s\n", message);  // Voorbeelduitvoer: Hoi, ik ben Alex, en ik ben 29 jaar oud.

    return 0;
}
```

## Diepere duik:

C is altijd een beetje hands-on geweest met strings. Interpolatie is geen directe functie - in plaats daarvan gebruiken we plaatsaanduiders en opmaakspecificaties zoals `%s` voor strings of `%d` voor cijfers binnen functies als `printf` of `sprintf`.

Historisch gezien zijn ontwikkelaars in andere talen verwend geraakt met interpolatiefuncties. In C# zou je gewoon `var message = $"Hallo, {name}!";` doen. Swift en Ruby hebben ook hun eigen extraatjes.

Terug in onze C wereld, doen opmaakspecificaties het zware werk. Onthoud, `sprintf` kan riskant zijn als je de buffergrootte verkeerd krijgt en je kan eindigen met het overschrijden van je buffer (`snprintf` is veiliger vanwege de bufferlimiet). Daarbij betekent dit handmatig beheer meer controle - een geliefd principe in de C ethos.

Er is meer dan één manier om een ​​kat te villen: bibliotheken van derden, zoals GLib, introduceren betere stringfunctionaliteiten. Functies zoals `g_strdup_printf` werken vergelijkbaar met `sprintf` maar beheren de geheugenallocatie voor je.

Variadic macros en functies kunnen ook interpolatie simuleren, maar dat is een geavanceerde handeling die een goede beheersing van macros en het `va_list` type uit `<stdarg.h>` vereist.

## Zie ook:

- ISO/IEC Programmeringstalen — C: https://www.iso.org/standard/74528.html
- C11 Standaard (N1570): http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf
- Documentatie van de GNU C Bibliotheek (glibc) over `printf`: https://www.gnu.org/software/libc/manual/html_node/Formatted-Output-Functions.html
- GLib's stringutiliteitsfuncties: https://developer.gnome.org/glib/stable/glib-Strings.html
