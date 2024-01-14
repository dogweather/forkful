---
title:                "PHP: Utskrift av feilrettingsutdata"
programming_language: "PHP"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du noen gang har støtt på en feil mens du programmerer i PHP, har du sikkert brukt print eller echo for å få ut debug output. Men hvorfor er det en nyttig teknikk å lære? 

## Hvordan
Å printe debug output er en enkel måte å finne ut hva som skjer i koden din. Det kan hjelpe deg med å identifisere hvor feilen ligger og hva som forårsaker den.

```PHP
<?php 
// Enkelt eksempel 
$navn = "Maria";
print "Hei, mitt navn er $navn"; 
```

```output
Hei, mitt navn er Maria
```

Du kan også printe ut verdier i variabler for å se hva som skjer med dem i løpet av koden din. Dette kan hjelpe deg med å forstå hvorfor koden din ikke fungerer som forventet. 

```PHP
<?php 
// Eksempel med variabel
$tall = 10;
print "Tallet er $tall";
```

```output
Tallet er 10
```

## Dypdykk
Det finnes flere metoder for å printe debug output i PHP, inkludert die(), var_dump() og error_log(). Disse metodene kan være nyttige i ulike situasjoner, for eksempel når du jobber med kompleks kode eller i et større prosjekt.

Det er også viktig å huske på å begrense bruken av print og echo, da for mye debug output kan gjøre koden din tregere og vanskeligere å lese.

## Se også
- [PHP.net - Debugging](https://www.php.net/manual/en/debugger.php)
- [PHP Debugging Tips](https://www.tutorialspoint.com/php/php_debugging.htm)
- [5 Debugging Tips for PHP Developers](https://www.sitepoint.com/5-debugging-tips-php-developers/)