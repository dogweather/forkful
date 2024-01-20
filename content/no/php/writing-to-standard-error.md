---
title:                "Skriving til standard feil"
html_title:           "PHP: Skriving til standard feil"
simple_title:         "Skriving til standard feil"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Å skrive til standard error er en vanlig praksis blant programmerere for å rapportere feil og annen relevant informasjon til terminalen eller loggfiler. Dette gir en mer pålitelig og strukturert måte å feilsøke og analysere programmet på.

# Hvordan:

Kodings eksempler og utgangsresultat 
```PHP
<?php
fwrite(STDERR, "Dette er en feilmelding.");
```
Utgang:
```
Dette er en feilmelding.
```

# Dypdykk:

Å skrive til standard error er en del av det å bruke stderr-funksjonen i PHP, som returnerer en åpen håndtak til standard error-strømmen. Denne praksisen er en del av standardisert Unix-programmering, der stderr brukes til å skrive ut feilmeldinger. Alternativer til å skrive til standard error inkluderer bruk av loggfiler eller å sende feilmeldinger til standard output (stdout). Implementeringen av denne funksjonen kan variere avhengig av programmeringsspråk og operativsystem.

# Se også:

- [PHP Manualens sider om Writing to Standard Error](https://www.php.net/manual/en/function.fwrite.php)
- [Unix Documentation om Standard Output and Standard Error](https://pubs.opengroup.org/onlinepubs/9699919799/functions/stdout.html)