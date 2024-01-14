---
title:    "PHP: Extrahera delsträngar"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Varför

Att extrahera substrängar är en vanlig uppgift i programmering och kan vara användbart för att dela upp en lång sträng i mindre delar eller för att söka efter ett visst mönster inuti en sträng. Det är också ett bra sätt att lära sig mer om strängmanipulation i allmänhet.

## Hur man gör det

Det finns flera olika sätt att extrahera substrängar i PHP, beroende på dina behov. Här är några exempel på hur du kan göra det:

```PHP
// Extrahera en del av strängen från en given position till slutet
$str = "Hej alla vänner!";
echo substr($str, 5); // Utmatning: "alla vänner!"

// Extrahera en del av strängen från en given position med en given längd
echo substr($str, 4, 4); // Utmatning: "alla"

// Extrahera en del av strängen baserat på ett mönster
$email = "john.doe@example.com";
$user = substr($email, 0, strpos($email, "@")); // Utmatning: "john.doe"
```

Som du kan se i det sista exemplet, kan du också använda en kombination av andra strängfunktioner som `strpos()` för att hitta substrängar baserat på ett visst mönster.

## Deep Dive

En av de vanligaste användningarna av substrängar är att söka efter ett visst mönster i en sträng eller att dela upp en lång sträng i mindre delar. PHP erbjuder flera andra funktioner som kan utföra liknande uppgifter, som `explode()`, `preg_match()` och `str_replace()`. Det är viktigt att förstå skillnaderna mellan dessa funktioner och välja rätt en för dina behov.

En annan intressant användning av substrängar är att manipulera sökvägar i filsystemet. PHP erbjuder en mängd olika filsystemrelaterade funktioner som också kan dra nytta av substrängar, som `basename()`, `dirname()` och `realpath()`.

## Se även

- [PHP substring Dokumentation](https://www.php.net/manual/en/function.substr.php)
- [PHP strängfunktioner Dokumentation](https://www.php.net/manual/en/ref.strings.php)
- [PHP filsystemfunktioner Dokumentation](https://www.php.net/manual/en/ref.filesystem.php)