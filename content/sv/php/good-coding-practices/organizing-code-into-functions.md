---
date: 2024-01-26 01:11:40.201558-07:00
description: "Att organisera kod i funktioner handlar om att dela upp din kod i \xE5\
  teranv\xE4ndbara block med definierade syften. Vi g\xF6r det f\xF6r att h\xE5lla\
  \ saker och ting\u2026"
lastmod: '2024-03-13T22:44:38.001904-06:00'
model: gpt-4-1106-preview
summary: "Att organisera kod i funktioner handlar om att dela upp din kod i \xE5teranv\xE4\
  ndbara block med definierade syften."
title: Att organisera kod i funktioner
weight: 18
---

## Hur man gör:
Föreställ dig att vi har upprepande kod för att hälsa på användare. Istället kommer vi att kapsla in den i en funktion som `greet_user`:

```php
function greet_user($name) {
    return "Hej, " . $name . "!";
}

echo greet_user("Alice");
echo greet_user("Bob");
```

Utskrift:
```
Hej, Alice!
Hej, Bob!
```

Nu har du ett praktiskt verktyg som du kan använda när som helst utan att behöva skriva om samma kodrader varje gång du vill säga hej.

## Fördjupning
Funktioner har varit en del av programmering sedan de tidiga dagarna av FORTRAN på 50-talet. De är en grundsten i strukturerad programmering och handlar allt om modularitet och isolering. Alternativ? Nåväl, du kan välja objektorienterad programmering och tala om klasser och metoder, vilka är funktioner med en fin kostym på. När det gäller PHP inkluderar genomföringsdetaljerna att specificera standardvärden för parametrar, typindikation för inmatningar och möjligheten att returnera flera värden genom att använda en array eller, från PHP 7.1 och framåt, en lista.

Här är en modern vändning med typdeklaration och standardvärden:

```php
function add(float $a, float $b = 0.0): float {
    return $a + $b;
}

echo add(1.5);
echo add(1.5, 2.5);
```

PHP 7.4 introducerade också pilfunktioner, vilket hjälper till att skriva koncisa enradiga funktioner, ofta använda i array-operationer:

```php
$numbers = array(1, 2, 3, 4);
$squared = array_map(fn($n) => $n * $n, $numbers);
print_r($squared);
```

Utskrift:
```
Array
(
    [0] => 1
    [1] => 4
    [2] => 9
    [3] => 16
)
```

## Se även
- [PHP-manualen om funktioner](https://www.php.net/manual/en/functions.user-defined.php)
- [PHP: Det Rätta Sättet - Funktioner](https://phptherightway.com/#functions)
- [Lär dig om PHP 7.4-pilfunktioner](https://stitcher.io/blog/short-closures-in-php)
