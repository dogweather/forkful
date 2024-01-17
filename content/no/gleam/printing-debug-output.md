---
title:                "Utskrift av feilsøkingsutdata"
html_title:           "Gleam: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?

Å skrive ut feilsøkingsutdata er en måte for programmører å få informasjon om hvordan programmet deres kjører. Dette kan hjelpe dem med å finne og rette feil i koden sin. Det er også nyttig for å forstå hva som skjer i programmet og hvordan dataene endrer seg underveis.

Slik gjør du:

For å skrive ut feilsøkingsutdata i Gleam, kan du bruke funksjonen `io.format`. For eksempel:

```Gleam
io.format("Debugging output: {} \n", ["Hello, world!"])
```

Output:

```
Debugging output: Hello, world!
```

Dykk ned i detaljene:

Skriving av feilsøkingsutdata har vært en høyt elsket metode blant programmører i mange år. Selv om det finnes alternative metoder som for eksempel å bruke en interaktiv debugger, er det fortsatt en enkel og effektiv måte å få innsikt i programmet ditt på. I Gleam, kan du også bruke `log`-funksjonen for å skrive ut meldinger, men `io.format` gir deg mer kontroll over formateringen og utdataen.

Se også:

- [Gleam dokumentasjon](https://gleam.run/book/tutorials/logging.html)
- [En oversikt over feilsøkingsmetoder i programmering](https://www.freecodecamp.org/news/7-debugging-techniques-any-developer-should-know/)
- [Diskusjon om fordelene og ulempene ved å skrive ut feilsøkingsutdata](https://stackoverflow.com/questions/885908/good-way-to-log-code)