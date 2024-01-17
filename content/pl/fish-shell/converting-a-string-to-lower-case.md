---
title:                "Zamiana ciągu znaków na małe litery"
html_title:           "Fish Shell: Zamiana ciągu znaków na małe litery"
simple_title:         "Zamiana ciągu znaków na małe litery"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Konwersja ciągu znaków na małe litery jest powszechnym zadaniem w programowaniu, polegającym na zmianie wszystkich znaków alfanumerycznych w ciągu na ich odpowiadające małe litery. Programiści często wykonują tę operację w celu ujednolicenia danych lub porównywania tekstów niezależnie od wielkości liter.

## Jak to zrobić:

Fish Shell oferuje wbudowaną funkcję `string tolower`, która pozwala na łatwe i szybkie konwertowanie ciągu znaków na małe litery. Przykładowe zastosowanie tej funkcji wyglądałoby w ten sposób:

```Fish Shell
set nazwaUzytkownika "JaKiś CiąG ZnAków"
set nazwaGotowa (string tolower $nazwaUzytkownika)
echo $nazwaGotowa
```

To polecenie wyświetli "jakis ciag znakow" w konsoli. Możesz również użyć tej funkcji bez ustawiania zmiennej, jeśli chcesz tylko wyświetlić przekonwertowany ciąg, na przykład:

```Fish Shell
echo (string tolower "JAkIŚ cIąG zNaKów")
```

Wynikiem będzie "jakis ciag znakow".

## Deep Dive:

Konwersja ciągu znaków na małe litery jest bardzo pomocna, ponieważ ułatwia porównywanie tekstów bez względu na wielkość liter. Jest również powszechnym rozwiązaniem problemów związanych z niedopasowaniem znaków, szczególnie przy wykorzystaniu języków, które nie mają jednoznacznych reguł dotyczących wielkości liter.

Alternatywnym sposobem na konwersję ciągu znaków na małe litery jest użycie pętli i operacji na pojedynczych znakach w danym języku programowania. Jednak wbudowana funkcja `string tolower` w Fish Shell zapewnia szybką i wygodną metodę wykonania tego zadania.

W implementacji, funkcja `string tolower` wykorzystuje funkcję na poziomie systemu operacyjnego odpowiedzialną za konwersję znaków, co pozwala na uniwersalne działanie na różnych systemach.

## Zobacz też:

- [Dokumentacja Fish Shell o funkcji `string tolower`](https://fishshell.com/docs/current/cmds/string-tolower.html)
- [Inne przydatne funkcje wbudowane w Fish Shell](https://fishshell.com/docs/current/cmds/index.html)