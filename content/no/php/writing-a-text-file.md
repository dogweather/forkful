---
title:                "Skriving av en tekstfil"
html_title:           "PHP: Skriving av en tekstfil"
simple_title:         "Skriving av en tekstfil"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Skriving av tekstfiler er en vanlig oppgave for mange programmerere. En tekstfil er rett og slett et dokument som inneholder tekst, uten formatering eller bilder. Det kan være mange grunner til å skrive en tekstfil, som å lagre data eller produsere utdata som skal leses av andre programmer.

# Slik gjør du det:

```PHP
$file = fopen("tekstfil.txt", "w") or die("Kan ikke opprette fil!");
$txt = "Dette er en tekstfil som skal skrives til tekstfil.txt";
fwrite($file, $txt);
fclose($file);
```

I dette eksemplet åpner vi en ny fil med navnet "tekstfil.txt" og angir at vi vil skrive til den ("w"). Vi sjekker også etter eventuelle feil og skriver en enkel feilmelding om det skulle være noe galt. Deretter skriver vi teksten "Dette er en tekstfil som skal skrives til tekstfil.txt" til filen og lukker den.

# Graving dypere:

Det er flere grunner til at det kan være nyttig eller nødvendig å skrive tekstfiler i programmeringsverdenen. En av de viktigste er å lagre data som kan gjenbrukes senere, for eksempel brukerinnstillinger eller loggfiler. Det finnes også alternativer til å skrive tekstfiler, som å lagre data i en database, men det kan være en mer kompleks løsning og ikke alltid nødvendig. Når det kommer til å skrive tekstfiler, er det viktig å være oppmerksom på at filen må eksistere på forhånd og at du har riktig tilgang til å skrive til den.

# Se også:

- [PHP: fopen](https://www.php.net/manual/en/function.fopen.php)
- [PHP: fwrite](https://www.php.net/manual/en/function.fwrite.php)
- [PHP: fclose](https://www.php.net/manual/en/function.fclose.php)