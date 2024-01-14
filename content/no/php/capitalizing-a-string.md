---
title:    "PHP: Stor bokstav i en streng"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å endre bokstavene i en streng (eller tekst) til store bokstaver kan være nyttig når man for eksempel ønsker å lage titteltekst eller markere viktig informasjon.

## Hvordan

Kapitalisering av en streng kan gjøres på flere måter i PHP. En metode er å bruke funksjonen "strtoupper", som gjør om alle bokstavene i en streng til store bokstaver. Se eksempel nedenfor:

```PHP
$input = "dette er en streng med små bokstaver";
$uppercase = strtoupper($input);
echo $uppercase; // Dette er en streng med små bokstaver
```

En annen måte å kapitalisere en streng på er å bruke funksjonen "ucwords", som gjør om første bokstav i hvert ord til stor bokstav. Se eksempel nedenfor:

```PHP
$input = "dette er en streng med små bokstaver";
$capitalized = ucwords($input);
echo $capitalized; // Dette Er En Streng Med Små Bokstaver
```

Man kan også lage sin egen funksjon for å kapitalisere en streng, som tar hensyn til unntaksord og andre spesifikke krav. Se eksempel nedenfor:

```PHP
function capitalizeString(string $input) {
    $exceptions = ["og", "i", "for", "av", "på"]; // legg til egne unntaksord her
    $words = explode(" ", $input); // deler opp strengen i ord

    foreach ($words as $key=>$word) {
        if (!in_array($word, $exceptions)) { // sjekker om ordet er et unntaksord
            $words[$key] = ucfirst($word); // gjør første bokstav til stor bokstav
        }
    }

    return implode(" ", $words); // setter sammen ordene igjen til en streng
}

$input = "dette er en streng med små bokstaver";
$customCapitalized = capitalizeString($input);
echo $customCapitalized; // Dette er en Streng med Små Bokstaver
```

## Deep Dive

Når man skal kapitalisere en streng, er det viktig å være klar over at bokstavene som skal endres må være i riktig format. Det vil si at de må være enten ASCII eller UTF-8 format. Dette kan påvirke resultatet hvis strengen inneholder spesialtegn eller bokstaver fra andre språk enn engelsk.

I tillegg er det også viktig å sjekke om det finnes allerede-eksisterende funksjoner som kan kapitalisere strenger på en bedre måte enn det man selv kan lage.

## Se Også

- "PHP string functions" (https://www.w3schools.com/php/php_ref_string.asp)
- "strtoupper PHP documentation" (https://www.php.net/manual/en/function.strtoupper.php)
- "ucwords PHP documentation" (https://www.php.net/manual/en/function.ucwords.php)