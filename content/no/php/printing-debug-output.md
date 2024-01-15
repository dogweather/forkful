---
title:                "Utskrift av feilanalyse"
html_title:           "PHP: Utskrift av feilanalyse"
simple_title:         "Utskrift av feilanalyse"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor ville noen bruke tid og energi på å skrive ut debug-utdata? Svaret er enkelt: det er en nyttig måte å feilsøke og finne feil i koden din. Ved å skrive ut variabler, funksjoner og prosesser underveis, kan du enkelt identifisere og løse potensielle problemer.

## Slik gjør du det
Det å skrive ut debug-utdata i PHP er enkelt og krever bare noen få kodeforskrifter. Du kan bruke funksjonen "echo" for å skrive ut enkel tekst, og funksjonen "print_r" for å skrive ut en mer kompleks array eller objekt. Se på eksemplene nedenfor for å få en bedre forståelse av hvordan det fungerer.

```PHP
$navn = "Ole";
$alder = 25;
echo "Navnet mitt er " . $navn . " og jeg er " . $alder . " år gammel.";
```

Dette vil skrive ut følgende:

`Navnet mitt er Ole og jeg er 25 år gammel.`

Hvis du ønsker å skrive ut hele innholdet i en array eller et objekt, kan du bruke "print_r" funksjonen på følgende måte:

```PHP
$frukt = array("eple", "banan", "jordbær");
print_r($frukt);
```

Dette vil skrive ut følgende:

```
Array
(
    [0] => eple
    [1] => banan
    [2] => jordbær
)
```

Dette kan være svært nyttig når du jobber med store og komplekse datastrukturer, og vil hjelpe deg med å se hvordan dataene er organisert og om det er noen feil.

## Dykk dypere
Det finnes flere metoder for å skrive ut debug-utdata, som for eksempel å bruke "var_dump" eller "debug_backtrace" funksjonene. Disse kan gi mer detaljert informasjon om variabler og funksjoner, men kan også være mer forvirrende å lese. 

Det kan også være lurt å bruke betingelser sammen med debug-utdata for å sjekke om visse deler av koden ble utført eller ikke. For eksempel kan du skrive ut en melding hvis en betingelse ikke ble oppfylt, noe som kan hjelpe deg med å finne ut hvor problemet ligger.

Når du skriver ut debug-utdata, er det viktig å huske å fjerne den når du har løst problemet. Du vil ikke at ekstra utdata skal bremse ned nettsiden din eller avsløre sensitiv informasjon til brukere.

## Se også
- [PHP.net - Debugging techniques](https://www.php.net/manual/en/debugger.php)
- [PHP Debugging techniques for beginners](https://www.tutorialrepublic.com/php-tutorial/php-debugging-techniques.php)
- [Debugging PHP with Xdebug](https://blog.jetbrains.com/phpstorm/2012/03/debugging-php-with-xdebug/)