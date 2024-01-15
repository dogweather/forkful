---
title:                "Utskrift av feilsøkingsutdata"
html_title:           "Bash: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Vi har sikkert alle vært i situasjoner der vi har kjørt en Bash-skript og ingen ting skjer. Det eneste vi får er en enkel feilmelding, uten noen hint om hva som går galt. Dette er når printing av debug output kan komme til unnsetning. Det lar oss se hva som skjer bak kulissene, og hjelper oss med å finne og løse feil.

## Hvordan gjøre det

For å printe debug output i Bash, bruk `echo`-kommandoen. Du kan enten skrive ut teksten direkte, eller bruke variabler for å få informasjon fra skriptet.

```
#!/bin/bash

# Printer ut teksten "Hello World!"
echo "Hello World!" 

# Bruker en variabel
name="John"
echo "Hei $name, velkommen til min skript!"
```

Output:

```
Hello World!
Hei John, velkommen til min skript!
```

En annen måte å printe ut debug output er ved å bruke `set -x` kommandoen på toppen av skriptet. Dette vil vise hvert steg i skriptet og resultatet av hvert kommando.

```
#!/bin/bash

set -x

# Printer ut teksten "Hello World!"
echo "Hello World!" 

# Bruker en variabel
name="John"
echo "Hei $name, velkommen til min skript!"
```

Output:

```
+ echo 'Hello World!'
Hello World!
+ name=John
+ echo 'Hei John, velkommen til min skript!'
Hei John, velkommen til min skript!
```

## Dypdykk

Når man printer debug output, er det viktig å huske på å ikke utsette sensitiv informasjon som passord, personlig informasjon eller systeminformasjon. Det kan også være nyttig å bruke en `if`-setning for å bare printe informasjonen hvis en bestemt betingelse er oppfylt.

En annen måte å få mer detaljert debug output er ved å bruke `set -xv`kommandoen, som vil vise hvert trinn i skriptet, inkludert verdiene til variablene.

```
#!/bin/bash

set -xv

# Printer ut teksten "Hello World!"
echo "Hello World!" 

# Bruker en variabel
name="John"
echo "Hei $name, velkommen til min skript!"
```

Output:

```
+ echo 'Hello World!'
Hello World!
+ name=John
+ echo 'Hei John, velkommen til min skript!'
Hei John, velkommen til min skript!
```

## Se også

- [Bash Guide for Nybegynnere](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Bash-programmeringsreferanse](https://tldp.org/LDP/abs/html/)