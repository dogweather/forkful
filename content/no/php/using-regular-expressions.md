---
title:                "PHP: Å bruke regulære uttrykk"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er en PHP-programmerer, har du mest sannsynligvis hørt om regular expressions. Disse uttrykkene er en kraftig verktøy for å behandle tekststrenger. De lar deg søke etter spesifikke mønstre i en streng, noe som gjør det enklere å finne og behandle data. Hvis du ønsker å bli en mer effektiv programmerer, bør du lære hvordan du bruker regular expressions i PHP.

## Hvordan

For å bruke regular expressions i PHP, må du først bruke ```preg_match()``` -funksjonen. Denne sjekker en streng mot et gitt uttrykk og returnerer enten sann eller usann, avhengig av om uttrykket matcher eller ikke. Her er et eksempel på hvordan du kan bruke det:

```PHP
$name = "John Doe";
if (preg_match("/Doe$/", $name)) {
  echo "Din etternavn er Doe!";
}
```

I dette eksempelet bruker vi ```preg_match()``` for å sjekke om strengen inneholder "Doe" på slutten. Hvis det er tilfelle, vil vi få utskrevet en melding som bekrefter dette.

Det finnes en rekke forskjellige uttrykk du kan bruke i regular expressions, for eksempel meta-tegn, spesielle tegn og flagg. Det er viktig å ha en god forståelse av disse for å utnytte fullt ut potensialet til regular expressions. Det finnes mange ressurser på nettet som kan hjelpe deg med å lære mer om dette.

## Dypdykk

Regular expressions kan virke komplekse i begynnelsen, men med litt øving og forståelse vil de bli en uvurderlig ressurs i PHP-programmeringen din. Du kan bruke dem til å validere brukerinput, filtrere data, og mye mer. En nyttig funksjon i PHP er også ```preg_replace()```, som lar deg erstatte deler av en streng med et annet uttrykk eller tekst.

En nyttig måte å lære mer om regular expressions på er å eksperimentere med dem selv og se hva som fungerer og ikke fungerer. Det finnes også mange gode verktøy på nettet, som for eksempel regex101.com, hvor du kan teste uttrykkene dine og få hjelp til å forstå dem.

## Se også

- [PHP regular expressions dokumentasjon](https://www.php.net/manual/en/function.preg-match.php)
- [RegExr - et online verktøy for å teste regular expressions](https://regexr.com/)
- [Tutorial: PHP regular expressions for beginners](https://www.tutorialspoint.com/php/php_regular_expression.htm)