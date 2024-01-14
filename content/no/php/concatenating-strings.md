---
title:    "PHP: Sammenføyning av strenger"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Når du jobber med PHP-programmering, vil det ofte være nødvendig å kombinere eller legge sammen forskjellige tekststrenger. Dette kan være for å lage en komplett setning eller for å formatere data på en bestemt måte. Uansett årsaken, er kunnskap om hvordan man konkatinerer strenger viktig for å skrive effektiv og funksjonell kode.

## Hvordan

En måte å konkatinere strenger i PHP er gjennom bruken av operatøren "." (punktsymbolet). Dette symbolet brukes til å legge sammen to strenger, og resultatet vil være en ny streng som inneholder den kombinerte teksten. La oss se på et enkelt eksempel:

```PHP
$navn = "Maria";
$beskjed = "Hei, mitt navn er " . $navn . "og jeg er fra Norge.";
echo $beskjed;
```

Dette vil gi følgende utskrift:

Hei, mitt navn er Maria og jeg er fra Norge.

I dette tilfellet ble variablene $navn og $beskjed sammenføyd ved hjelp av parten ".". Merk at mellomrom må være inkludert for å unngå å få sammenflettede ord.

En annen måte å kombinere strenger på er gjennom bruken av PHP-funksjonen "sprintf()". Denne funksjonen tar imot en tekststreng og variabler som skal settes inn i strengen på spesifiserte plasseringer. La oss se på et eksempel:

```PHP
$navn = "Tobias";
$trope = "Hawaier";
$beskjed = sprintf("Hei, mitt navn er %s og jeg drømmer om å bli en ekte %s.", $navn, $trope);
echo $beskjed;
```

Dette vil gi følgende utskrift:

Hei, mitt navn er Tobias og jeg drømmer om å bli en ekte Hawaier.

I dette tilfellet brukes "%s" som plassholdere for variablene $navn og $trope. Dette gjør det enklere å sette inn variabler i en tekststreng uten å måtte bekymre seg for å legge til ekstra mellomrom eller andre tegn.

## Dypdykk

Det finnes også andre måter å konkatinere strenger i PHP, som for eksempel gjennom bruken av "strcat()" eller "str_replace()" funksjonene. Disse har sine egne spesifikke bruksområder og kan være nyttige i ulike situasjoner. Det viktigste er å forstå grunnleggende om hvordan å koble sammen strenger, og deretter forstå og bruke de forskjellige metodene som passer best for ditt spesifikke kodebehov.

## Se Også

1. [PHP funksjoner for strenger](https://www.php.net/manual/en/ref.strings.php)
2. [W3Schools - PHP konkatinering](https://www.w3schools.com/php/php_operators.asp)
3. [Stack Overflow - Sammenføyning av strenger i PHP](https://stackoverflow.com/questions/368657/string-concatenation-in-php)