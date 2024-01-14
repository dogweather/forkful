---
title:    "Bash: Å finne lengden til en streng"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden på en streng er en vanlig oppgave når man jobber med Bash-programmering. Det kan være nyttig å vite lengden på en streng for å kunne utføre ulike operasjoner på den, som for eksempel sjekke om den er tom eller begrense antall tegn som skal vises. I denne blogginnlegget skal vi se på hvordan man kan finne lengden på en streng i Bash, med praktiske eksempler.

## Hvordan

Det finnes flere måter å finne lengden på en streng i Bash på. En av de enkleste måtene er å bruke parameterutvidelsen `${#string}`. Her er et eksempel:

```Bash
string="Hei alle sammen!"
echo "Lengden på strengen er: ${#string}"
```

Dette vil gi følgende output: 

```
Lengden på strengen er: 17
```

En annen metode er å bruke kommandoen `expr length string`:

```Bash
string="Hei alle sammen!"
echo "Lengden på strengen er: $(expr length "$string")"
```

Dette vil gi samme output som det første eksempelet.

## Dypdykk

Det er viktig å merke seg at både `expr` og parameterutvidelsen `${#string}` kun fungerer på en enkelt streng. Hvis strengen inneholder mellomrom, vil disse metodene kun telle tegnene før det første mellomrommet. Hvis du trenger å finne lengden på en streng som inneholder mellomrom, kan du bruke `echo -n string | wc -c`. `-n` flagget i `echo` kommandoen gjør at det ikke blir lagt til et nytt linjeskift etter strengen, mens `wc -c` teller antall tegn.

## Se også

- [Bash dokumentasjon - parameterutvidelse](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [Bash dokumentasjon - expr kommando](https://www.gnu.org/software/bash/manual/html_node/Shell-Arithmetic.html#Shell-Arithmetic)
- [wc kommando dokumentasjon](https://www.gnu.org/software/coreutils/manual/html_node/wc-invocation.html)