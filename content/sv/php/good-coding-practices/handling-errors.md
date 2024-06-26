---
date: 2024-01-26 00:55:36.541894-07:00
description: "Hur g\xF6r man: I PHP kan du hantera fel med hj\xE4lp av `try-catch`-block,\
  \ och du kan anpassa processen med egna felhanterare och undantag."
lastmod: '2024-03-13T22:44:38.003732-06:00'
model: gpt-4-1106-preview
summary: "I PHP kan du hantera fel med hj\xE4lp av `try-catch`-block, och du kan anpassa\
  \ processen med egna felhanterare och undantag."
title: Hantering av fel
weight: 16
---

## Hur gör man:
I PHP kan du hantera fel med hjälp av `try-catch`-block, och du kan anpassa processen med egna felhanterare och undantag.

```php
// Grundläggande exempel på try-catch
try {
  // Gör något riskfyllt
  $file = fopen("nonexistentfile.txt", "r");
} catch (Exception $e) {
  // Hantera felet
  echo "Fel: " . $e->getMessage();
}

// Ställa in en egen felhanterare
set_error_handler(function($severity, $message, $file, $line) {
  throw new ErrorException($message, 0, $severity, $file, $line);
});

// Använda undantag
class MyException extends Exception {}

try {
  // Gör något och kasta ett anpassat undantag
  throw new MyException("Anpassat fel!");
} catch (MyException $e) {
  // Hantera det anpassade undantaget
  echo $e->getMessage();
}

// Exempel på utmatning:
// Fel: fopen(nonexistentfile.txt): misslyckades med att öppna strömmen: Ingen sådan fil eller katalog
// Anpassat fel!
```

## Djupdykning
Förr handlade PHP-fel mer om varningar och meddelanden som inte stoppade skriptkörningen. När språket mognade antog det en mer robust objektorienterad felhantering genom Exception-klassen som introducerades i PHP 5. Senare kom PHP 7 med felklasser som äntligen skilde på fel och undantag.

Innan `try-catch`-block använde PHP `set_error_handler()` för att hantera fel. `try-catch` är renare, mer modernt. Men egna felhanterare har fortfarande en plats, särskilt för äldre kod eller när du behöver fånga upp vad som normalt skulle vara icke-undantagsfel.

Gränssnittet `Throwable` i PHP 7+ innebär att oavsett om det är ett Error eller Exception kan du fånga båda. Detta är praktiskt eftersom du nu inte missar kritiska körningsfel, som var svårare att spåra tidigare.

Alternativ utanför PHP:s inbyggda mekanismer innefattar bibliotek och ramverk som kommer med sina egna felhanteringssystem, och erbjuder fler funktioner som felloggning till filer eller att visa användarvänliga felmeddelanden.

## Se också
- Officiell PHP-dokumentation om undantag: https://www.php.net/manual/en/language.exceptions.php
- PHP The Right Way om felrapportering: https://phptherightway.com/#error_reporting
- PHP-handboken om Felhantering: https://www.php.net/manual/en/book.errorfunc.php
