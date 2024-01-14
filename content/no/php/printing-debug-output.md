---
title:                "PHP: Utskrift av feilsøkingsmeldinger"
simple_title:         "Utskrift av feilsøkingsmeldinger"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du er en PHP-utvikler, har du kanskje lagt merke til at mange av PHP-rammeverkene og bibliotekene bruker en funksjon som `echo` for å skrive ut feilsøkingsinformasjon. Men hvorfor er det egentlig viktig å skrive ut feilsøkingsinformasjon? Svaret er enkelt - det kan hjelpe deg med å finne og løse feil i koden din raskere.

## Hvordan
I PHP kan du enkelt skrive ut feilsøkingsinformasjon ved hjelp av funksjonen `echo` eller `print`. Her er et eksempel:

```PHP
$navn = "Maria";
echo "Navnet er: " . $navn;
```

Dette vil skrive ut følgende:

```
Navnet er: Maria
```

Du kan også bruke `var_dump()` for å skrive ut informasjon om variabler og til og med arrays. Her er et eksempel:

```PHP
$array = array(1, 2, 3);
var_dump($array);
```

Dette vil skrive ut følgende:

```
array(3) {
  [0]=>
  int(1)
  [1]=>
  int(2)
  [2]=>
  int(3)
}
```

Som du kan se, kan feilsøkingsinformasjon som dette være veldig nyttig for å finne og fikse feil i koden din.

## Deep Dive
Det er også verdt å merke seg at i tillegg til å skrive ut feilsøkingsinformasjon, kan du også bruke `error_reporting` i PHP for å få mer detaljert informasjon om eventuelle feil som oppstår i koden din. Ved å sette `error_reporting` til `E_ALL`, vil du kunne se alle typer feil, inkludert advarsler og notices.

Du kan også bruke `isset()` og `empty()` funksjonene for å sjekke om en variabel er initialisert eller har en verdi. Dette kan være nyttig for å unngå uventede feil i koden din.

## Se også
- PHP manual: [https://www.php.net/manual/en/ref.var.php](https://www.php.net/manual/en/ref.var.php)
- Stack Overflow: [https://stackoverflow.com/questions/tagged/php](https://stackoverflow.com/questions/tagged/php)
- PHP Developer Blog: [https://www.phpdeveloper.org/](https://www.phpdeveloper.org/)