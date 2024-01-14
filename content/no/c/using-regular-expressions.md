---
title:    "C: Å bruke regulære uttrykk"
keywords: ["C"]
---

{{< edit_this_page >}}

## Hvorfor

Hvis du har programmert i C i en stund, har du sikkert opplevd å måtte håndtere en mengde tekst og strenger. Kanskje du har måttet finne et bestemt mønster i en tekst eller utføre komplekse søk og erstatninger. I slike tilfeller kan det å bruke vanlige uttrykk (regular expressions) være en stor hjelp. Med vanlige uttrykk kan du raskt finne og manipulere tekst basert på mønstre, og det kan være en effektiv måte å håndtere store mengder data på.

## Hvordan

For å bruke vanlige uttrykk i C-programmering, må du inkludere headerfilen `<regex.h>` i koden din. Deretter kan du bruke funksjonene `regcomp()`, `regexec()` og `regfree()` for å kompilere, matche og frigjøre de vanlige uttrykkene dine.

For eksempel, hvis du ønsker å finne alle ord som begynner på en bestemt bokstav i en tekst, kan du bruke følgende kode:

```C 
#include <stdio.h>
#include <regex.h>

int main() {
    char str[] = "En nydelig dag i Norge.";
    regex_t regex;
    int reti;

    reti = regcomp(&regex, "\b[aA]\w+", 0);
    if (reti) {
        printf("Kunne ikke kompilere vanlig uttrykk.\n");
        return 1;
    }
    reti = regexec(&regex, str, 0, NULL, 0);
    if (!reti) {
        printf("Fant et passende ord: %s\n", str);
    } else if (reti == REG_NOMATCH) {
        printf("Ingen treff på tekst.\n");
    } else {
        printf("Feil ved matchen.\n");
        return 1;
    }
    regfree(&regex);
    return 0;
}
```

Dette eksemplet vil skrive ut "Fant et passende ord: Norge." siden "Norge" er det eneste ordet som begynner med en stor eller liten "a" i teksten.

## Dypdykk

Vanlige uttrykk i C støtter en rekke ulike metakarakterer og spesielle sekvenser, som gjør at du kan lage mer komplekse mønstre. For eksempel kan du bruke `^` for å matche et mønster som kun befinner seg i starten av teksten, og `$` for et mønster som befinner seg på slutten. Du kan også bruke `*` for å matche et mønster som kan gjenta seg null eller flere ganger.

Det er også mulig å bruke grupper i vanlige uttrykk, som lar deg fange og behandle deler av en matchet tekst separat. For å bruke dette, må du bruke parenteser i det vanlige uttrykket ditt, og referere til gruppen med `\1`, `\2` osv. i en passende funksjon.

Det finnes flere ressurser som kan hjelpe deg med å lære mer om vanlige uttrykk og deres bruksområder. En av de mest nyttige er "Regular Expressions - The Complete Guide" av Jan Goyvaerts og Steven Levithan.

## Se også

- [Regular Expressions - The Complete Guide](https://www.amazon.com/Regular-Expressions-Mastering-Power-Ballene/dp/0596528124)
- [Regex Tutorial: Matching Characters](https://www.regular-expressions.info/characters.html)
- [Using Regular Expressions in C](https://www.thegeekstuff.com/2019/07/c-regex-examples/)

Takk for at du leste denne bloggposten. Vi håper den har gitt deg en god innføring i hvordan du kan bruke vanlige uttrykk i C-programmering. Lykke til med å håndtere tekstmengdene dine!