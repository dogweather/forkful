---
title:                "Radera tecken som matchar ett mönster"
html_title:           "PHP: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att radera tecken som matchar ett mönster är en användbar funktion i PHP för att manipulera och hantera strängar. Det kan vara användbart vid till exempel förberedelse av data för bearbetning eller för att ta bort oönskade tecken i en sträng.

## Hur man gör det

För att radera tecken som matchar ett visst mönster i en sträng i PHP kan du använda funktionen `preg_replace()`. Denna funktion tar in tre parametrar: mönstret som ska matchas, ersättningen för mönstret och strängen som ska manipuleras.

Ett enkelt exempel på hur `preg_replace()` kan användas för att ta bort alla siffror från en sträng ser ut så här:

```PHP
$string = "Jag är 27 år gammal.";
$ny_sträng = preg_replace("/[0-9]/", "", $string);
echo $ny_sträng;

// Output: Jag är år gammal.
```

I detta exempel används mönstret `"/[0-9]/"`, vilket betyder att alla siffror i strängen kommer att matchas och ersättas med en tom sträng. Detta mönster kan sedan anpassas efter dina behov för att matcha olika tecken eller teckenkombinationer.

## Djupdykning

För att förstå mer om hur `preg_replace()` fungerar är det viktigt att förstå vad reguljära uttryck är. Reguljära uttryck är mönster som används för att matcha textsträngar och filtrera ut önskad information. I PHP används de ofta tillsammans med funktionen `preg_replace()` för att manipulera strängar.

Som vi ser i exemplet ovan används hakparenteser `[ ]` för att specifiera en grupp av tecken som ska matchas. Inuti dessa hakparenteser kan man även ange ett intervall av tecken, såsom `[a-z]` för att matcha alla små bokstäver. Andra specialtecken som ofta används i reguljära uttryck är till exempel `^` för att matcha början av en sträng, `$` för att matcha slutet av en sträng och `+` för att matcha en eller flera förekomster av ett tecken.

En annan viktig del av `preg_replace()` är ersättningsparametern. I vårt exempel använde vi en tom sträng, men du kan även ersätta det matchande mönstret med en annan sträng eller till och med en funktion som returnerar en sträng. Det finns också olika modifierare som kan användas för att ändra beteendet hos `preg_replace()`, såsom `i` för att göra matchningen icke-skiftlägeskänslig eller `g` för att matcha flera förekomster av mönstret.

## Se även

- Mer information om reguljära uttryck i PHP: https://www.php.net/manual/en/regexp.reference.php
- Allmän information om `preg_replace()`: https://www.php.net/manual/en/function.preg-replace.php