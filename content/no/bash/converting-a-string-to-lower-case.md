---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Bash: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang lurt på hvordan du kan konvertere en streng til små bokstaver i Bash-programmering? Her vil vi vise deg hvorfor det er nyttig og hvordan du kan gjøre det.

## Slik gjør du det

Så, hvordan kan du enkelt konvertere en streng til små bokstaver i Bash? Heldigvis har Bash en innebygd funksjon som heter "tr" som står for "translate" og den gjør akkurat det vi trenger. Se på eksempelet nedenfor:

```Bash
string="HELLO WORLD"
echo "Original streng: $string" # Original streng: HELLO WORLD
string=$(echo $string | tr '[:upper:]' '[:lower:]')
echo "Konvertert streng: $string" # Konvertert streng: hello world
```

La oss bryte ned koden. Først definerer vi en variabel "string" som inneholder en streng i store bokstaver. Deretter bruker vi "echo" for å skrive ut den originale strengen. Deretter tilordner vi variabelen "string" på nytt ved å kjøre et subshell med "echo" og rører resultatet til "tr" kommandoen. "tr" kommandoen spesifiserer at vi vil oversette alle store bokstaver til små bokstaver ved å bruke "[upper:]" og "[lower:]" uttrykkene. Til slutt skriver vi ut den konverterte strengen.

Du kan også konvertere en streng til små bokstaver ved hjelp av "sed" kommandoen på følgende måte:

```Bash
string="HELLO WORLD"
echo "Konvertert streng: $(echo $string | sed 's/./\L&/g')" # Konvertert streng: hello world
```

"sed" kommandoen tar nytte av "&" tegnet som representerer den første bokstaven i en streng og "\L" for å gjøre om det til små bokstaver.

## Dypdykk

Denne metoden for å konvertere en streng til små bokstaver i Bash er nyttig når du arbeider med tekst og ønsker å konvertere den til en ensartet form. Det kan også være nyttig når du skal sammenligne strenger fordi det eliminerer forskjellene mellom store og små bokstaver.

Det er viktig å merke seg at "tr" kommandoen bare fungerer med ASCII-tegnsett. Hvis du trenger å arbeide med ikke-ASCII-tegn, kan du bruke "sed" kommandoen som støtter multibyte-tegn. Du kan også bruke andre kommandoer og metoder for å konvertere en streng til små bokstaver i Bash, men "tr" og "sed" er de mest effektive og enkle å forstå.

## Se også

- [Bash dokumentasjon](https://www.gnu.org/software/bash/)
- [tr kommandoen](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [sed kommandoen](https://www.gnu.org/software/sed/)