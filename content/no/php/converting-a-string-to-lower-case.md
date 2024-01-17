---
title:                "Konvertere en streng til små bokstaver"
html_title:           "PHP: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Konvertering av en streng til små bokstaver betyr å endre alle bokstavene i en streng til deres tilsvarende små bokstaver. Dette gjøres for å sikre en enhetlig formatering og for å gjøre det enklere å sammenligne strenger i koden.

# Hvordan:
Enkelt sagt, for å omgjøre en streng til små bokstaver i PHP, bruker vi funksjonen `strtolower()`. Dette gir oss en ny streng med alle bokstavene i små bokstaver. Her er et eksempel på hvordan dette fungerer:

```PHP
$string = "HeLlO";
echo strtolower($string); // Output: hello
```

# Dypdykk:
Konvertering av en streng til små bokstaver har eksistert siden de tidlige dagene av programmering. I gamle dager måtte man manuelt endre bokstavene en etter en for å få riktig formatering. I dag har de fleste programmeringsspråk en innebygd funksjon for å gjøre dette, inkludert PHP. Alternativet til `strtolower()` er `strtoupper()`, som gjør det samme, men med store bokstaver istedenfor.

Implementeringen av `strtolower()` funksjonen bruker Unicode-bokstaver for å sikre at alle språk støttes. Dette gjør at funksjonen er pålitelig og nøyaktig uavhengig av hvilket språkmiljø koden din kjører på.

# Se også:
Hvis du ønsker å lære mer om andre nyttige strengfunksjoner i PHP, kan du sjekke ut dokumentasjonen her: [PHP String Functions](https://www.php.net/manual/en/ref.strings.php). Du kan også lære mer om Unicode og hvorfor det er viktig for programmerere her: [What is Unicode?](https://www.w3.org/International/questions/qa-what-is-unicode).