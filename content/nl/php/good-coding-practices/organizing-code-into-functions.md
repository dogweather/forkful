---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:16.203847-07:00
description: "Hoe: Stel je voor dat we herhalende code hebben voor het begroeten van\
  \ gebruikers. In plaats daarvan zullen we het inpakken in een functie zoals\u2026"
lastmod: '2024-03-13T22:44:50.901030-06:00'
model: gpt-4-0125-preview
summary: Stel je voor dat we herhalende code hebben voor het begroeten van gebruikers.
title: Code organiseren in functies
weight: 18
---

## Hoe:
Stel je voor dat we herhalende code hebben voor het begroeten van gebruikers. In plaats daarvan zullen we het inpakken in een functie zoals `greet_user`:

```php
function greet_user($name) {
    return "Hallo, " . $name . "!";
}

echo greet_user("Alice");
echo greet_user("Bob");
```

Uitvoer:
```
Hallo, Alice!
Hallo, Bob!
```

Nu heb je een handig hulpmiddel dat je altijd kunt gebruiken zonder elke keer dezelfde regels code opnieuw te schrijven wanneer je hallo wilt zeggen.

## Diepe Duik
Functies zijn al in programmeren sinds de vroege dagen van FORTRAN in de jaren '50. Ze zijn een hoeksteen van gestructureerd programmeren en gaan helemaal over modulariteit en isolatie. Alternatieven? Nou, je kunt objectgeoriënteerd gaan en klassen en methoden bespreken, wat functies zijn met een mooi pak aan. Wat PHP betreft, omvatten implementatiedetails het specificeren van standaardwaarden voor parameters, type hinting voor invoer, en de mogelijkheid om meerdere waarden terug te geven door gebruik te maken van een array of, vanaf PHP 7.1, een lijst.

Hier is een moderne draai met typeverklaring en standaardwaarden:

```php
function add(float $a, float $b = 0.0): float {
    return $a + $b;
}

echo add(1.5);
echo add(1.5, 2.5);
```

PHP 7.4 introduceerde ook arrow functies, die helpen om beknopte eenregelige functies te schrijven, vaak gebruikt bij array-operaties:

```php
$numbers = array(1, 2, 3, 4);
$squared = array_map(fn($n) => $n * $n, $numbers);
print_r($squared);
```

Uitvoer:
```
Array
(
    [0] => 1
    [1] => 4
    [2] => 9
    [3] => 16
)
```

## Zie Ook
- [PHP Handboek over Functies](https://www.php.net/manual/en/functions.user-defined.php)
- [PHP: De Juiste Manier - Functies](https://phptherightway.com/#functions)
- [Leer over PHP 7.4 Arrow Functies](https://stitcher.io/blog/short-closures-in-php)
