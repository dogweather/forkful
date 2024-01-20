---
title:                "Zamiana liter na wielkie w ciągu znaków"
html_title:           "Fish Shell: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Zamiana literek na duże, jest procesem, który polega na przekształcaniu małych liter w duże. Programiści robią to, aby zwiększyć czytelność kodu i różnicować pewne elementy, takie jak stałe.

## Jak to zrobić:

Kodowanie w Fish Shell jest proste i przejrzyste. Na przykład, aby zmienić literki na duże w stringu, używamy wbudowanej funkcji `string upper`.
```fish
fish> set my_string 'hello world'
fish> string upper $my_string
HELLO WORLD
```
W powyższym kodzie, pierwsza linia tworzy zmienną `my_string` z wartością 'hello world'. Druga linia używa funkcji `string upper`, aby zmienić wszystkie literki w `my_string` na duże.

## Głębsze zrozumienie

Funkcja `string upper` jest dostępna w shellu Fish od wersji 2.3.0. W porównaniu z innymi shellami, Fish oferuje proste i czytelne funkcje manipulacji stringami.

Istnieją również alternatywne metody kapitalizacji stringów, takie jak użycie `awk` lub `tr`, ale wymagają one bardziej skomplikowanych konstrukcji.

Jeśli chodzi o szczegóły implementacji, `string upper` w Fish działa przez iterowane zamienianie każdego znaku na duży, używając biblioteki C.

## Zobacz też

Jeśli chcesz zgłębić temat, oto kilka linków do powiązanych źródeł, które mogą Ci pomóc:
- Dokumenty Fish Shell: https://fishshell.com/docs/current/
- Tutorial Fish Shell na GitHubie: https://github.com/jorgebucaran/fish-shell-cookbook