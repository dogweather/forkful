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

## Vad & Varför?

Att ta bort tecken som matchar ett mönster är en viktig del av programmering. Det innebär helt enkelt att ta bort specifika tecken från en textsträng baserat på ett angivet mönster. Detta kan vara användbart för att rensa en text på oönskade tecken eller för att utvinna specifika delar av information från en sträng.

## Så här gör du:

Ett vanligt sätt att ta bort tecken som matchar ett mönster i PHP är att använda funktionen `preg_replace()`. I exemplet nedan tar vi bort alla siffror från en textsträng.

```PHP
$text = "Det finns 123 äpplen i korgen.";
$ny_text = preg_replace("/[0-9]/", "", $text);
echo $ny_text;
// Output: Det finns äpplen i korgen.
```

Notera att mönstret som vi matchar är `[0-9]`, vilket betyder alla siffror från 0 till 9. Det andra argumentet är en tom sträng, vilket innebär att alla matchande tecken kommer att tas bort från vår ursprungliga sträng. Slutligen skriver vi ut vår nya sträng för att se resultatet.

## Djupt dykande:

Ta bort tecken som matchar ett mönster är en del av det som kallas reguljära uttryck eller Regex. Detta koncept är inte specifikt för PHP utan finns i många andra programmeringsspråk också. Regex kan användas för att matcha och manipulera text på ett mycket mer avancerat sätt än det enkla exempel vi har sett här.

En annan metod för att ta bort tecken som matchar ett mönster är att använda funktionen `str_replace()`. Detta är en enklare metod som bara tar bort exakta tecken eller strängar, i motsats till Regex som kan hantera mer komplexa mönster.

Att använda `preg_replace()` kan vara lite mer resurskrävande och fungerar bäst på mindre strängar. Om du behöver bearbeta större mängder data kan det vara mer effektivt att använda `str_replace()`.

## Se även:

- PHP's dokumentation för `preg_replace()`: https://www.php.net/manual/en/function.preg-replace.php
- En introduktion till Regex: https://www.regular-expressions.info/
- En utförlig guide om PHP och Regex: https://www.php.net/manual/en/reference.pcre.pattern.syntax.php