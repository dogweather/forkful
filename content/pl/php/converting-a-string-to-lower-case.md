---
title:    "PHP: Konwertowanie ciągu znaków na małe litery"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Dlaczego?

Istnieje wiele sytuacji, w których konwersja ciągu znaków na małe litery jest niezbędna w programowaniu PHP. Może to być konieczne, aby upewnić się, że dane wejściowe są w odpowiednim formacie, umożliwić porównywanie ciągów bezwzględnych lub po prostu dla celów formatowania tekstu.

## Jak to zrobić?

Aby przekonwertować ciąg znaków na małe litery w PHP, można użyć funkcji `strtolower()`. Przyjmuje ona jeden argument - ciąg znaków, który chcemy przekonwertować. Poniżej znajduje się przykładowy kod, który ilustruje to działanie:

```PHP
$tekst = "TO JEST PRZYKŁADOWY TEKST";
echo strtolower($tekst);
```

Po uruchomieniu powyższego kodu otrzymamy następujący wynik:

`to jest przykładowy tekst`

Jak widać, funkcja `strtolower()` zamieniła wszystkie litery na małe. W przypadku, gdy chcemy przekonwertować ciąg znaków na wielkie litery, możemy użyć funkcji `strtoupper()`.

## Dogłębna analiza

Warto zauważyć, że konwersja ciągu znaków na małe litery jest zależna od ustawień regionalnych serwera. Oznacza to, że w innych językach niż angielski, wynik konwersji może być inny. Dla przykładu, w języku tureckim litera "I" jest konwertowana na "ı", a nie na "i", jak ma to miejsce w języku angielskim.

Ponadto, istnieje również funkcja `mb_strtolower()`, która jest dostępna w bibliotece `mbstring` i działa poprawnie dla wielu języków. Jest to szczególnie przydatne w przypadku pracy z tekstem zawierającym znaki spoza zakresu ASCII.

## Zobacz też

- Oficjalna dokumentacja PHP w języku polskim: [https://www.php.net/manual/pl/](https://www.php.net/manual/pl/)
- Porównanie wydajności funkcji `strtolower()` i `mb_strtolower()`: [https://stackoverflow.com/questions/12422467/speed-of-mb-string-vs-non-mb-string-functions-in-php](https://stackoverflow.com/questions/12422467/speed-of-mb-string-vs-non-mb-string-functions-in-php)
- Metoda manipulacji tekstami w PHP: [https://www.w3schools.com/php/php_string.asp](https://www.w3schools.com/php/php_string.asp)