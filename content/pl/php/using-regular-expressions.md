---
title:    "PHP: Używanie wyrażeń regularnych"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Każdy programista PHP powinien nauczyć się wykorzystywać wyrażenia regularne w swoim kodzie. Pozwalają one na precyzyjną manipulację ciągami znaków, co jest niezbędne w wielu projektach.

## Jak używać wyrażeń regularnych w PHP

Wyrażenia regularne są obsługiwane przez funkcje wbudowane w PHP, w tym funkcje `preg_match()`, `preg_replace()` i `preg_split()`. Wszystkie przyjmują trzy argumenty: wyrażenie regularne, ciąg znaków do sprawdzenia i opcje.

Przykładowo, jeśli chcemy znaleźć wszystkie wystąpienia słowa "hello" w ciągu znaków, możemy użyć funkcji `preg_match()` w ten sposób:

```PHP
$str = "Hello world! Witaj świecie!";
if (preg_match("/hello/i", $str)) { // /i oznacza ignorowanie wielkości liter
  echo "Znaleziono słowo hello!";
} else {
  echo "Nie znaleziono słowa hello.";
}
```

To wywołanie funkcji sprawdzi, czy w ciągu znaków `$str` znajduje się wyrażenie regularne `hello`, zignorując wielkość liter. W tym przypadku zostanie wyświetlony komunikat "Znaleziono słowo hello!".

Można również zastosować wyrażenie regularne do zamiany części ciągu znaków, używając funkcji `preg_replace()`. Na przykład, jeśli chcemy zamienić wszystkie wystąpienia słowa "Cześć" na "Hello", możemy użyć tego kodu:

```PHP
$str = "Cześć Maciek! Witaj w świecie programowania.";
$newStr = preg_replace("/cześć/i", "Hello", $str);
echo $newStr; // Wyświetli: Hello Maciek! Witaj w świecie programowania.
```

## Głębsze zagadnienia dotyczące wyrażeń regularnych

Korzystając z wyrażeń regularnych, można znacznie usprawnić przetwarzanie danych w aplikacjach PHP. Można na przykład użyć wyrażeń regularnych do:

- Weryfikacji poprawności adresu email lub numeru telefonu
- Sprawdzania poprawności formatu daty
- Wyszukiwania i zastępowania części tekstu
- Dzielenia tekstu na mniejsze części

Warto poświęcić więcej czasu na poznanie składni i możliwości wyrażeń regularnych, aby móc wykorzystać je w swoich projektach w pełni.

## Zobacz również

- [Dokumentacja PHP na temat funkcji regulárních výrazů](https://www.php.net/manual/en/ref.pcre.php)
- [Tutorial na temat wyrażeń regularnych w PHP](https://www.tutorialspoint.com/php/php_regular_expression.htm)
- [Wyrażenia regularne w 10 minut](https://www.youtube.com/watch?v=EkluES9Rvak) - przydatny filmik dla początkujących programistów.