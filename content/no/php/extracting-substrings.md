---
title:                "PHP: Uttrekking av substrings"
simple_title:         "Uttrekking av substrings"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å trekke ut delstrenger fra en tekststreng kan være utrolig nyttig når du arbeider med PHP-programmering. Dette kan hjelpe deg med å manipulere tekst på en mer effektiv måte, noe som sparer deg for både tid og frustrasjon. Hvis du ønsker å lære hvordan du kan trekke ut delstrenger i dine PHP-prosjekter, har du kommet til rett sted.

## Hvordan

For å trekke ut delstrenger, kan du bruke PHPs built-in funksjoner `substr()` og `mb_substr()`. Disse funksjonene gjør det mulig å angi start- og sluttposisjoner for delstrengen du ønsker å trekke ut.

```PHP
// Her trekker vi ut de første 5 tegnene fra en tekststreng

$text = "Dette er en tekststreng";
echo substr($text, 0, 5); // Output: Dette
```

Du kan også bruke disse funksjonene til å trekke ut delstrenger basert på en bestemt posisjon i tekststrengen. For eksempel kan du bruke `mb_substr()` til å trekke ut delstrenger fra en bestemt posisjon til slutten av tekststrengen.

```PHP
// Her trekker vi ut delstrenger fra posisjon 10 og til slutten av tekststrengen

$text = "Dette er en tekststreng";
echo mb_substr($text, 10); // Output: tekststreng
```

## Dypdykk

Hvis du ønsker å utforske mer avanserte måter å trekke ut delstrenger på, kan du ta en titt på funksjonene `preg_match()` og `preg_match_all()`. Disse funksjonene bruker regulære uttrykk for å trekke ut delstrenger fra en tekststreng basert på et bestemt mønster.

For eksempel kan du bruke `preg_match()` til å trekke ut telefonnumre fra en tekststreng ved å anvende et regulært uttrykk som matcher på et typisk telefonnummer-format.

## Se også

For mer informasjon om å trekke ut delstrenger i PHP, kan du sjekke ut disse nyttige ressursene:

- [PHP manual - substr()](https://www.php.net/manual/en/function.substr.php)
- [PHP manual - mb_substr()](https://www.php.net/manual/en/function.mb-substr.php)
- [PHP manual - preg_match()](https://www.php.net/manual/en/function.preg-match.php)
- [PHP manual - preg_match_all()](https://www.php.net/manual/en/function.preg-match-all.php)