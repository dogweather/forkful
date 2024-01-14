---
title:    "PHP: Utskrift av feilsøkningsutdata"
keywords: ["PHP"]
---

{{< edit_this_page >}}

# Hvorfor

Når man programmerer, kan det være utfordrende å finne ut hvorfor en kode ikke fungerer som den skal. Det er her å skrive ut debug-meldinger kommer godt med. Dette gjør det enklere å finne feil og løse problemer.

# Slik gjør du det

For å skrive ut debug-meldinger i PHP, kan du bruke funksjonen `echo` eller `print`. Disse funksjonene tar inn en eller flere variabler som argumenter, og skriver ut verdien av disse.

```PHP
$navn = "Ole";
$alder = 25;
echo "Navn: " . $navn . "<br>";
echo "Alder: " . $alder;
```

Dette vil gi følgende output:

```
Navn: Ole
Alder: 25
```

Du kan også bruke `var_dump` for å få mer detaljert informasjon om variabler, som for eksempel type og størrelse.

```PHP
$tall = 5;
var_dump($tall);
```

Dette vil gi følgende output:

```
int(5)
```

# Dypdykk

Det finnes også andre måter å skrive ut debug-meldinger på i PHP, som for eksempel `error_log` funksjonen som sender en feilmelding til serverens error logg. Du kan også bruke conditional statements for å begrense når debug-meldinger skal skrives ut, for eksempel ved å sjekke om en bestemt betingelse er oppfylt.

# Se også

- https://www.php.net/manual/en/function.echo.php
- https://www.php.net/manual/en/function.print.php
- https://www.php.net/manual/en/function.var-dump.php
- https://www.php.net/manual/en/function.error-log.php