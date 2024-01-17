---
title:                "Generering av tilfeldige tall"
html_title:           "PHP: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?
Generering av tilfeldige tall er en vanlig oppgave i programmering. Dette er en måte å skape en tilfeldig verdi som kan brukes i et program. Programmene våre trenger ofte å håndtere tilfeldige situasjoner, og det å kunne generere tilfeldige tall tillater oss å simulere disse situasjonene og gjøre programmet mer variert og dynamisk.

## Hvordan:
Her er et eksempel på hvordan du kan generere et tilfeldig tall i PHP:

```PHP
$tilfeldig_tall = rand(1, 10); // Her genereres et tilfeldig tall mellom 1 og 10
echo $tilfeldig_tall; // Output: 7
```

Du kan også generere tilfeldige tall fra et gitt utvalg ved å bruke funksjonen `array_rand ()`:

```PHP
$alternativer = array("Apple", "Banana", "Orange", "Grape");
$tilfeldig_frukt = $alternativer[array_rand($alternativer)]; // Her velges en tilfeldig frukt fra listen
echo $tilfeldig_frukt; // Output: Apple (kan variere)
```

## Dypdykk:
Å generere tilfeldige tall kan spores helt tilbake til 1930-tallet med utviklingen av Monte Carlo-simuleringer for å løse matematiske problemer. I tillegg til `rand ()` og `array_rand ()` -funksjonene, er `mt_rand ()` -funksjonen også tilgjengelig i PHP, som er en raskere versjon av `rand ()`.

Et alternativ til å bruke innebygde PHP-funksjoner er å bruke en tredjeparts tilfeldighetsgenereringsklasse, for eksempel random_compat. Denne klassen gir viderekomne funksjoner for å generere tilfeldige tall og er også kompatibel med eldre versjoner av PHP.

For en sikrere tilnærming til tilfeldig tallgenerering, kan du bruke den kryptografiske sikre metoden `random_bytes ()` og `random_int ()` funksjonene som er introdusert i PHP 7.

## Se også:
- [PHP dokumentasjon for tilfeldige tall](https://www.php.net/manual/en/function.rand.php)
- [random_compat library](https://github.com/paragonie/random_compat)
- [PHP dokumentasjon for random_bytes og random_int](https://www.php.net/manual/en/function.random-bytes.php)