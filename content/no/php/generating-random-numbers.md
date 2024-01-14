---
title:                "PHP: Generering av tilfeldige tall"
programming_language: "PHP"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Generering av tilfeldige tall er en viktig del av mange programmer og spill, og kan være nyttig for å skape variasjon og spenning. Det er også et viktig konsept å forstå for å kunne skrive robust og sikker kode.

## Slik gjør du det

```PHP
// Generere et tilfeldig tall mellom 1 og 10
$randomNumber = rand(1,10);

// Skrive ut det tilfeldige tallet
echo "Det tilfeldige tallet er: " . $randomNumber;

// Resultat: Det tilfeldige tallet er: (random tall mellom 1 og 10)
```

```PHP
// Generere et tilfeldig desimaltall mellom 0 og 1
$randomDecimal = mt_rand() / mt_getrandmax();

// Skrive ut det tilfeldige desimaltallet
echo "Det tilfeldige desimaltallet er: " . $randomDecimal;

// Resultat: Det tilfeldige desimaltallet er: (random desimaltall mellom 0 og 1)
```

Her har vi brukt de innebygde PHP-funksjonene "rand()" og "mt_rand()" for å generere tilfeldige tall. "rand()" genererer et heltall mellom to gitt verdier, mens "mt_rand()" genererer et desimaltall mellom 0 og 1. Det finnes også andre funksjoner og metoder for å generere tilfeldige tall, så det er viktig å lese dokumentasjonen nøye for å velge den riktige for ditt formål.

Det er også mulig å sette et "seed" i tilfeldighetsgeneratoren for å få de samme tallene hver gang koden kjøres. Dette kan være nyttig for testing og debugging av koden din. For eksempel:

```PHP
// Sette et "seed" på 12345 for tilfeldighetsgeneratoren
mt_srand(12345);

// Generere et tilfeldig tall mellom 1 og 10
$randomNumber = mt_rand(1,10);

// Skrive ut det tilfeldige tallet
echo "Det tilfeldige tallet er: " . $randomNumber;

// Resultat: Det tilfeldige tallet er: 6
```

## Dykk dypere

Det kan være nyttig å forstå hvordan tilfeldighetsgeneratoren fungerer for å unngå vanlige feil og sikkerhetsrisikoer. De fleste språk, inkludert PHP, bruker en algoritme som simulerer tilfeldighet ved å bruke en "seed" som brukes som utgangspunkt for å generere tallene.

En vanlig feil er å la "seeden" være statisk, slik at den alltid er den samme. Dette kan føre til at tilfeldige tall blir forutsigbare og dermed sårbare for angrep. Det er derfor viktig å bruke en god "seed", som kan være for eksempel en kombinasjon av systemets klokkeslett og en unik brukeridentifikator.

## Se også

- [PHP manual: Generere tilfeldige tall](https://www.php.net/manual/en/function.rand.php)
- [PHP manual: Mersenne Twister tilfeldighetsgenerator](https://www.php.net/manual/en/function.mt-rand.php)
- [OWASP: Sikker generering av tilfeldige tall](https://owasp.org/www-project-cheat-sheets/cheatsheets/Insecure_Randomness_Cheat_Sheet.html)