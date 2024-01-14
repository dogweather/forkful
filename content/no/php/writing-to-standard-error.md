---
title:    "PHP: Skriver til standardfeil"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skrive til standardfeil? Dette er et vanlig spørsmål blant PHP-programmerere. Svaret er enkelt: det er en viktig måte å håndtere feil og feilsøke problemer i koden din.

Når du skriver til standardfeil, vil eventuelle feil eller advarsler som oppstår under kjøring av programmet ditt bli skrevet til en egen fil i stedet for å bli vist på skjermen. Dette gjør det enklere å finne og løse feil, spesielt hvis programmet ditt har mye utskrift og du ikke vil ha feilmeldinger blandet inn.

## Slik gjør du det

For å skrive til standardfeil, kan vi bruke funksjonen `fwrite` sammen med `STDERR`-konstanten. Her er et eksempel på hvordan koden vår kan se ut:

```PHP
$filename = "feillog.txt";

if (!is_writable($filename)) {
    die("Kan ikke skrive til filen: $filename");
}

$handle = fopen($filename, "a");
if (!$handle) {
    die("Kunne ikke åpne filen for skriving");
}

// Lager en testfeil
$error = "Dette er en testfeil.";
// Skriver feilen til standardfeil
fwrite(STDERR, $error);

fclose($handle);
```

Koden vår sjekker først om filen vi vil skrive til er skrivbar. Deretter åpner vi filen og skriver feilen vår til `STDERR`. Til slutt lukker vi filen igjen.

Nå kan vi se på innholdet i filen vår `feillog.txt`. Der vil vi finne meldingen vår: "Dette er en testfeil"!

```
Feillog: Dette er en testfeil.
```

Dette er bare et eksempel, du kan selvfølgelig utvide dette konseptet til å håndtere flere typer feil og logge dem til forskjellige filer.

## Dypdykk

Nå som vi vet hvordan vi kan skrive til standardfeil, kan vi se på noen fordeler og ulemper ved denne tilnærmingen.

En av de største fordelene er at det gjør det enklere å feilsøke problemer i et større, komplekst program. Med standardfeil kan du logge feil til en dedikert fil og lettere finne og analysere dem.

En ulempe er at dette ikke er en standard måte å håndtere feil på i PHP. Det kan føre til at andre utviklere som jobber med koden din, blir forvirret hvis de ikke er vant til denne tilnærmingen.

En annen ulempe er at det kan bli vanskeligere å finne feilmeldingene hvis de blir skrevet til en fil i stedet for å vises på skjermen. Du må kanskje lete gjennom flere filer for å finne feilen, noe som kan være tidkrevende.

## Se også

- [Håndtering av feil i PHP](https://www.php.net/manual/en/book.errorfunc.php)
- [Logge feil til fil i PHP](https://www.php.net/manual/en/function.error-log.php)
- [Feilsøking i PHP](https://www.php.net/manual/en/ref.errorfunc.php)