---
title:    "C: Å bruke regulære uttrykk"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skal du bry deg om regular expressions? Vel, hvis du ønsker å effektivisere søk og manipulasjon av tekst i dine C-programmer, er dette verktøyet for deg!

## Hvordan

For å bruke regular expressions i C-programmering må du inkludere libraryet `regex.h` i koden din. La oss se på et enkelt eksempel:

```C
#include <stdio.h>
#include <regex.h>

int main()
{
    // Opprette et regex-objekt med mønsteret "a*b"
    regex_t regex;
    int ret = regcomp(&regex, "a*b", 0);

    // Sjekke om "aaab" matcher mønsteret
    ret = regexec(&regex, "aaab", 0, NULL, 0);
    if (ret == 0)
        printf("Det er en match!");
    else if (ret == REG_NOMATCH)
        printf("Ingen match.");

    // Frigjøre minnet til regex-objektet
    regfree(&regex);
    return 0;
}
```

I dette eksempelet definerer vi et regex-objekt med mønsteret "a*b", som betyr at vi leter etter teksten "a" etterfulgt av null eller flere "b"-er. Deretter sjekker vi om teksten "aaab" matcher mønsteret. Hvis det gjør det, vil vi få beskjed om at det er en match, ellers vil vi få beskjed om at det ikke er en match. Til slutt frigjør vi minnet til regex-objektet for å unngå potensielle lekkasjer.

Du kan også bruke regular expressions i funksjoner som `regexec` og `regerror` for å få mer detaljert informasjon om en match eller eventuelle feil.

## Dypdykk

Regular expressions kan være litt tricky å forstå i begynnelsen, men når du blir vant til dem, vil du sette pris på hvor kraftig de er. Det finnes mange vanlige metakarakterer og spesielle syntakser for å lage mønstre, for eksempel `*` for å matche null eller flere forekomster, `+` for å matche én eller flere forekomster og `()` for å gruppere deler av mønsteret. Det er også mulig å bruke regex-objekter til å finne og bytte ut tekst i en streng.

Hvis du vil lære mer om regular expressions, anbefaler vi å se på dokumentasjonen til `regex.h` eller søke etter ressurser på nettet.

## Se også

* [Dokumentasjon for regex.h (på engelsk)](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
* [Regex Cheat Sheet (på engelsk)](https://www.rexegg.com/regex-quickstart.html)