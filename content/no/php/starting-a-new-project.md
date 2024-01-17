---
title:                "Å starte et nytt prosjekt"
html_title:           "PHP: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "PHP"
category:             "PHP"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/starting-a-new-project.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?
Å starte et nytt prosjekt i programmering betyr å begynne på et nytt program eller en applikasjon fra bunnen av. Dette gjøres ofte for å løse et spesifikt problem eller for å lage noe nytt og innovativt.

Hvordan:
For å starte et nytt prosjekt i PHP, er det første du må gjøre å sette opp en mappestruktur for prosjektet ditt. Dette kan gjøres ved å følge anerkjente standarder som PSR-4 eller ved å sette opp en mappestruktur som passer ditt spesifikke behov.

```PHP
<?php
// Eksempel på mappestruktur for et PHP-prosjekt
app/       // Inneholder applikasjonskoden
config/    // Inneholder konfigurasjonsfiler
public/    // Inneholder offentlige filer som kan nås via nettleseren
tests/     // Inneholder tester for koden
vendor/    // Inneholder eksterne biblioteker og avhengigheter
```

Etter å ha satt opp mappen, kan du begynne å kode applikasjonen din. For å starte et nytt PHP-prosjekt, må du lage en fil med en .php utvidelse og begynne å skrive koden din.

```PHP
<?php
// Hent ut et array med navn
$names = ["Kari", "Per", "Maria"];

// Gå gjennom hvert navn og skriv det ut på en ny linje
foreach ($names as $name) {
  echo $name . PHP_EOL;
}

// Eksempeloutput:
// Kari
// Per
// Maria
```

Deep Dive:
Å starte et nytt prosjekt i PHP er en vanlig praksis blant utviklere. Dette skyldes hovedsakelig PHPs fleksibilitet og funksjonalitet som gjør det enkelt å utvikle små og store prosjekter. Det finnes også alternative programmeringsspråk som kan brukes for å starte et nytt prosjekt, som for eksempel Python eller JavaScript.

Når du har begynt å skrive koden din, vil du kanskje også ønske å implementere et rammeverk som kan hjelpe deg med å strukturere og organisere koden din på en mer effektiv måte. Det finnes mange populære PHP-rammeverk som Laravel, CodeIgniter og Symfony som kan hjelpe deg med dette.

Se også:
- The Official PHP Documentation: https://www.php.net/manual/en/
- PSR-4 Standards: https://www.php-fig.org/psr/psr-4/
- Laravel Framework: https://laravel.com/
- CodeIgniter Framework: https://codeigniter.com/
- Symfony Framework: https://symfony.com/