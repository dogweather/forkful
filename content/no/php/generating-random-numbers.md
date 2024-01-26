---
title:                "Generering av tilfeldige tall"
date:                  2024-01-20T17:49:40.886457-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generering av tilfeldige tall"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å generere tilfeldige nummer tillater programmer å få ikke-forutsigbare resultater. Det er essensielt for alt fra å kaste terninger i spill til sikkerhetsmekanismer i kryptografi.

## Hvordan:
For å generere tilfeldige tall i PHP, bruk funksjonene `random_int()` for kryptografisk sikre verdier, eller `rand()` for mer grunnleggende behov. Her er eksempler:

```PHP
// Sikker tilfeldig nummergenerering
$secureRandomNumber = random_int(1, 100);
echo "Sikker tilfeldig verdi: $secureRandomNumber\n";

// Basis tilfeldig nummergenerering
$basicRandomNumber = rand(1, 100);
echo "Basis tilfeldig verdi: $basicRandomNumber\n";
```

Output kan være:
```
Sikker tilfeldig verdi: 25
Basis tilfeldig verdi: 73
```

## Dykk dypere:
Tidligere, funksjoner som `rand()` og `mt_rand()` var standard for tilfeldig nummergenerering, men disse tilbød ikke tilstrekkelig sikkerhet for alle formål. I 2015, med PHP 7, introduceres `random_int()` og `random_bytes()`, som produserer kryptografisk sikre tilfeldige tall ved bruke av tilfeldig data hentet fra operativsystemet.

Alternativer for tilfeldig nummergenerering inkluderer libsodium (en kryptografisk bibliotek) for PHP-utviklere som trenger ekstra funksjonalitet utover det som er bygget inn i språket. Implementasjonsdetaljer varierer stort basert på om du trenger nummerene til å være forutsigbare (som når du tester kode) eller fullstendig tilfeldige (som for sikkerhetsformål).

## Se også:
For mer detaljerte opplysninger og beste praksis, sjekk ut følgende ressurser:
- PHP Manual on `random_int()`: https://www.php.net/manual/en/function.random-int.php
- PHP Manual on `rand()`: https://www.php.net/manual/en/function.rand.php
- Wikipedia om Pseudo-Random Number Generators (PRNGs): https://en.wikipedia.org/wiki/Pseudorandom_number_generator
- Libsodium for PHP: https://www.php.net/manual/en/book.sodium.php
