---
date: 2024-01-26 00:55:56.195757-07:00
description: "Hvordan: I PHP kan du h\xE5ndtere feil ved \xE5 bruke `try-catch`-blokker,\
  \ og du kan tilpasse prosessen med egendefinerte feilh\xE5ndterere og unntak."
lastmod: '2024-03-13T22:44:40.894073-06:00'
model: gpt-4-1106-preview
summary: "I PHP kan du h\xE5ndtere feil ved \xE5 bruke `try-catch`-blokker, og du\
  \ kan tilpasse prosessen med egendefinerte feilh\xE5ndterere og unntak."
title: "Feilh\xE5ndtering"
weight: 16
---

## Hvordan:
I PHP kan du håndtere feil ved å bruke `try-catch`-blokker, og du kan tilpasse prosessen med egendefinerte feilhåndterere og unntak.

```php
// Grunnleggende eksempel på try-catch
try {
  // Gjør noe risikabelt
  $file = fopen("nonexistentfile.txt", "r");
} catch (Exception $e) {
  // Håndtere feilen
  echo "Feil: " . $e->getMessage();
}

// Sette en egendefinert feilhåndterer
set_error_handler(function($severity, $message, $file, $line) {
  throw new ErrorException($message, 0, $severity, $file, $line);
});

// Bruke unntak
class MyException extends Exception {}

try {
  // Gjør noe og kast et egendefinert unntak
  throw new MyException("Egendefinert feil!");
} catch (MyException $e) {
  // Håndtere det egendefinerte unntaket
  echo $e->getMessage();
}

// Eksempel på utdata:
// Feil: fopen(nonexistentfile.txt): klarte ikke å åpne strømmen: Ingen slik fil eller katalog
// Egendefinert feil!
```

## Dykk dypere
Tidligere var PHP-feil mer om advarsler og merknader som ikke stoppet skriptkjøringen. Ettersom språket modnet, adopterte det en mer robust objektorientert feilhåndtering via Exception-klassen introdusert i PHP 5. Senere kom PHP 7 med Error-klasser som endelig skilte mellom feil og unntak.

Før `try-catch`-blokker brukte PHP `set_error_handler()` for å håndtere feil. `try-catch` er renere og mer moderne. Men egendefinerte feilhåndterere har fortsatt sin plass, spesielt for eldre kode eller når du trenger å fange opp det som vanligvis ville være ikke-unntaksfeil.

`Throwable`-grensesnittet i PHP 7+ betyr at enten det er en Error eller et Exception, kan du fange opp begge. Dette er hendig fordi nå går du ikke glipp av kritiske kjøretidsfeil, som var vanskeligere å spore før.

Alternativer utenfor PHPs innebygde mekanismer inkluderer biblioteker og rammeverk som kommer med egne feilhåndteringssystemer, som tilbyr flere funksjoner som feillogging til filer eller visning av brukervennlige feilsider.

## Se også
- Offisiell PHP-dokumentasjon om unntak: https://www.php.net/manual/en/language.exceptions.php
- PHP Den riktige måten om feilrapportering: https://phptherightway.com/#error_reporting
- PHP-manualen om feilhåndtering: https://www.php.net/manual/en/book.errorfunc.php
