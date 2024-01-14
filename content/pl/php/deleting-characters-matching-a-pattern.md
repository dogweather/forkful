---
title:    "PHP: Usuwanie znaków odpowiadających wzorcowi"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Czemu

Czasami, jako programiści, musimy przeprowadzać operacje na danych, aby je uporządkować lub dostosować do naszych potrzeb. Jedną z takich operacji jest usuwanie znaków zgodnych z pewnym wzorcem. Może to być potrzebne, gdy chcemy usunąć znaki specjalne z tekstu lub pozbyć się niepożądanych fragmentów.

## Jak to zrobić

Aby usunąć znaki zgodne z pewnym wzorcem w PHP, możemy użyć funkcji `preg_replace()`. Przyjmuje ona trzy argumenty - wzorzec, który chcemy znaleźć i usunąć, tekst, na którym chcemy przeprowadzić operację oraz tekst zastępujący usunięte znaki. Na przykład, jeśli chcemy usunąć wszystkie liczby z tekstu, możemy użyć następującego kodu:

```PHP
$text = "23 czerwca 2020 roku";
$text_bez_liczb = preg_replace("/[0-9]/", "", $text);
echo $text_bez_liczb;
```

Output: " czerwca rok"

Możemy również użyć tego sposobu do usuwania znaków specjalnych, jak np. spacja lub znaki końca linii. W takim przypadku wystarczy dodać odpowiednie znaki do wzorca.

## Głębszy zanurzenie

Aby lepiej zrozumieć i wykorzystać funkcję `preg_replace()` w PHP, warto zapoznać się z wyrażeniami regularnymi i ich zastosowaniem w programowaniu. Wyrażenia regularne pozwalają nam określić wzorce, których szukamy w tekście, a funkcja `preg_replace()` pozwala nam wykorzystać te wzorce do przeprowadzania operacji na tekście.

See Also:

- [Dokumentacja PHP - funkcja preg_replace()](https://www.php.net/manual/en/function.preg-replace.php)
- [Podstawy wyrażeń regularnych w PHP](https://www.w3schools.com/php/php_regex.asp)
- [Przewodnik po wyrażeniach regularnych w PHP](https://www.regular-expressions.info/php.html)