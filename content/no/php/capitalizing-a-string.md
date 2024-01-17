---
title:                "Å kapitalisere en streng"
html_title:           "PHP: Å kapitalisere en streng"
simple_title:         "Å kapitalisere en streng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å kapitalisere en streng i programmering betyr å gjøre den første bokstaven i hver eneste ord stor, mens resten av bokstavene forblir små. Dette er vanligvis gjort for å gi en bedre visuell presentasjon av en tekst. Programmerere gjør det for å få koden sin til å se mer ryddig ut og for å gjøre den lettere å lese.

## Hvordan:
```PHP
$string = "dette er en setning som skal kapitaliseres";
echo ucfirst($string); // Output: Dette er en setning som skal kapitaliseres
```

```PHP
$string = "dette er en annen setning som skal kapitaliseres for kun det første ordet";
echo ucwords($string); // Output: Dette Er En Annen Setning Som Skal Kapitaliseres For Kun Det Første Ordet
```

## Dypdykk:
Det å kapitalisere en streng har eksistert siden de tidlige dagene av programmering, og er ofte brukt i typiske programmeringsspråk som C og Java. Alternativet til å bruke de innebygde funksjonene i PHP er å lage din egen funksjon som utfører det samme, men dette kan være mer tidkrevende og mindre effektivt. Implementeringen av kapitaliseringsfunksjonene i PHP legger også til rette for flere språk, ettersom de tar hensyn til forskjellige regler for store og små bokstaver i hvert språk.

## Se Også:
[PHP ucfirst() funksjonen](https://www.php.net/manual/en/function.ucfirst.php)
[PHP ucwords() funksjonen](https://www.php.net/manual/en/function.ucwords.php)