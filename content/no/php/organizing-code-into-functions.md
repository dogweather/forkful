---
title:                "Organisering av kode i funksjoner"
date:                  2024-01-26T01:11:38.477086-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organisering av kode i funksjoner"

category:             "PHP"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å organisere kode i funksjoner handler om å dele opp koden din i gjenbrukbare blokker med definerte formål. Vi gjør dette for å holde ting ryddig, forhindre redundans, og gjøre feilsøking til en lek.

## Hvordan:
Tenk deg at vi har repetitiv kode for å hilse på brukere. I stedet vil vi pakke det inn i en funksjon som `greet_user`:

```php
function greet_user($name) {
    return "Hallo, " . $name . "!";
}

echo greet_user("Alice");
echo greet_user("Bob");
```

Output:
```
Hallo, Alice!
Hallo, Bob!
```

Nå har du et hendig verktøy du kan bruke når som helst uten å måtte skrive om de samme kodene hver gang du vil si hei.

## Dypdykk
Funksjoner har vært en del av programmering siden de tidlige dagene av FORTRAN på 50-tallet. De er en hjørnestein i strukturert programmering og handler all about modulæritet og isolasjon. Alternativer? Vel, du kan gå objektorientert og snakke om klasser og metoder, som er funksjoner med en fancy dress. Når det gjelder PHP, inneholder implementeringsdetaljer å spesifisere standardverdier for parametere, type hinting for inndata og muligheten til å returnere flere verdier ved å bruke en array eller, fra PHP 7.1 og utover, en liste.

Her er en moderne vri med typedeklarasjon og standardverdier:

```php
function add(float $a, float $b = 0.0): float {
    return $a + $b;
}

echo add(1.5);
echo add(1.5, 2.5);
```

PHP 7.4 introduserte også pilfunksjoner, noe som hjelper til med å skrive konsise en-linjers funksjoner, som ofte brukes i arrayoperasjoner:

```php
$numbers = array(1, 2, 3, 4);
$squared = array_map(fn($n) => $n * $n, $numbers);
print_r($squared);
```

Output:
```
Array
(
    [0] => 1
    [1] => 4
    [2] => 9
    [3] => 16
)
```

## Se også
- [PHP-manualen om funksjoner](https://www.php.net/manual/en/functions.user-defined.php)
- [PHP: The Right Way - Funksjoner](https://phptherightway.com/#functions)
- [Lær om PHP 7.4 Pilfunksjoner](https://stitcher.io/blog/short-closures-in-php)
