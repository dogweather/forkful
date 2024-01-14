---
title:    "C: Wydrukowanie wyjścia debugowego"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Jednym z najważniejszych narzędzi w procesie programowania jest wyświetlanie informacji na temat działania naszego kodu. Może to być pomocne w celu znalezienia błędów, zrozumienia przepływu programu lub po prostu monitorowania jego działania. W tym blogu dowiesz się, dlaczego wyświetlanie informacji debugowania jest ważne i jak to zrobić w C.

## Jak to zrobić

Aby wyświetlić informacje debugowania w języku C, możemy skorzystać z funkcji `printf()`. Wykorzystuje się ją do wypisywania różnych typów danych, takich jak tekst, liczby czy wartości logiczne.

Przykład:

```C
int num = 5;
printf("Wartość zmiennej \"num\" to %d\n", num);
```
Wyjście:
```
Wartość zmiennej "num" to 5
```
W powyższym przykładzie użyłem znaku specjalnego `%d`, który oznacza, że zmienna typu `int` zostanie wyświetlona jako liczba dziesiętna. Istnieje wiele innych znaków specjalnych, które można wykorzystać w celu wyświetlenia różnych typów danych. Kilka najważniejszych to:

- `%d` - liczba dziesiętna
- `%f` - liczba zmiennoprzecinkowa (float)
- `%c` - znak
- `%s` - tekst
- `%x` - liczba szesnastkowa

Należy również pamiętać, że można wyświetlić więcej niż jedną zmienną za pomocą jednej funkcji `printf()`. W tym celu wystarczy po prostu dodać kolejne zmienne po znaku `%`.

## Deep Dive

W języku C istnieje również funkcja `fprintf()`, która działa w podobny sposób jak `printf()`, ale pozwala wyświetlać informacje do pliku zamiast do konsoli. Jest to przydatne w przypadku, gdy chcemy zachować informacje debugowania dla późniejszego przejrzenia.

Istnieje również możliwość wyświetlania bardziej szczegółowych informacji, takich jak adresy pamięci, dzięki użyciu funkcji `sprintf()`. Jest to szczególnie przydatne w przypadku debugowania programów korzystających z dynamicznej alokacji pamięci.

Oprócz funkcji `printf()`, `fprintf()` i `sprintf()`, istnieje również wiele innych narzędzi i metod do wyświetlania informacji debugowania w języku C. Jest to obszerna dziedzina, która wymaga doświadczenia i eksperymentowania, aby osiągnąć najlepsze wyniki.

## Zobacz również

- [10 przydatnych wskazówek do debugowania w C](https://www.programiz.com/article/c-debugging-tips)
- [Dokumentacja funkcji `printf()`](https://www.cplusplus.com/reference/cstdio/printf/)
- [Poradnik: Wykorzystaj funkcję `fprintf()` do zapisywania informacji do pliku](https://fresh2refresh.com/c-programming/c-file-handling/fprintf-fputs-fscanf-fscanf-sprintf-scanf-cc-functions/)