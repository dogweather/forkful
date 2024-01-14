---
title:    "PHP: Wyszukiwanie i zamienianie tekstu"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Dlaczego warto używać wyszukiwania i zastępowania tekstu w programowaniu PHP?

W dzisiejszych czasach, programowanie jest niezbędnym narzędziem w wielu dziedzinach. Jednym z najważniejszych elementów programowania jest manipulacja tekstem. W przypadku języka PHP, często zdarza się, że musimy przeprowadzić masową zmianę w tekście. Może to być wymaganie wprowadzenia nowej funkcji, naprawienie literówki lub zastąpienie starych wyrażeń nowymi. W takich sytuacjach, wyszukiwanie i zastępowanie tekstu jest nie tylko ważne, ale również oszczędza czas i pozwala uniknąć błędów. W tym artykule dowiesz się, jak wykorzystać to narzędzie w programowaniu PHP.

## Jak to zrobić?

Wyszukiwanie i zastępowanie tekstu w PHP jest możliwe dzięki użyciu funkcji `str_replace()` lub `preg_replace()`. Pierwsza z nich jest prostsza i pozwala na proste wyszukiwanie i zastępowanie tekstu, podczas gdy druga wykorzystuje wyrażenia regularne i jest bardziej zaawansowana. Przejdźmy teraz do praktycznych przykładów.

```
// Proste wyszukiwanie i zastępowanie tekstu
$text = "Dziękuję za pomoc w programowaniu!";
echo str_replace("Dziękuję", "Wielkie dzięki", $text);
// Output: Wielkie dzięki za pomoc w programowaniu!

// Wyszukiwanie i zastępowanie tekstu z użyciem wyrażenia regularnego
$text = "Witaj, użytkowniku! Twój PIN to 1234.";
echo preg_replace("/PIN to \d+/", "PIN to ****", $text);
// Output: Witaj, użytkowniku! Twój PIN to ****.
```

W pierwszym przykładzie, funkcja `str_replace()` wyszukuje w tekście wyrażenie "Dziękuję" i zastępuje je frazą "Wielkie dzięki". W drugim przykładzie, `preg_replace()` wykorzystuje wyrażenie regularne, aby zastąpić numer PIN gwiazdkami.

## Głębsze zanurzenie

Wyszukiwanie i zastępowanie tekstu w PHP może być bardziej skomplikowane, zwłaszcza jeśli chcemy zastosować wiele zmian na jednym ciągu tekstu. W takich przypadkach, warto zapoznać się z dokumentacją funkcji `preg_replace()`, która pozwala na zaawansowane wyszukiwanie z wykorzystaniem wzorców i zmiennej tablicy zawierającej wyrażenia oraz ich zamienniki. Jest to szczególnie przydatne, gdy chcemy przeprowadzić masową zmianę w tekście, np. zmienić wszystkie daty na format DD/MM/RRRR.

## Zobacz również

- [Dokumentacja PHP na temat funkcji str_replace()](https://www.php.net/manual/en/function.str-replace.php)
- [Dokumentacja PHP na temat funkcji preg_replace()](https://www.php.net/manual/en/function.preg-replace.php)

Dzięki wykorzystaniu wyszukiwania i zastępowania tekstu w programowaniu PHP, możemy zaoszczędzić czas i uniknąć błędów. Pamiętaj, że jest to tylko jedno z narzędzi dostępnych w języku PHP, więc warto zgłębić również inne funkcje i możliwości tego języka.