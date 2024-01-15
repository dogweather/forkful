---
title:                "Zmiana wielkości litery w ciągu znaków"
html_title:           "Fish Shell: Zmiana wielkości litery w ciągu znaków"
simple_title:         "Zmiana wielkości litery w ciągu znaków"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego
Jedną z podstawowych operacji, które musimy wykonywać w programowaniu, jest manipulacja napisami. Jedną z często używanych jest zmiana wielkości liter – zwłaszcza w celu uzyskania poprawnego formatowania. W tym artykule dowiesz się, jak używając Fish Shell, możesz prostym sposobem zamienić napis na jego zdatną do wykorzystania formę.

## Jak to zrobić?
Fish Shell oferuje wbudowaną funkcję `string toupper` do zmiany wszystkich liter w napisie na duże oraz `string tolower` do zmiany na małe. Możesz również użyć funkcji `string capitalize`, aby zamienić pierwszą literę w napisie na dużą, pozostawiając pozostałe bez zmian.

```Fish Shell
echo "witaj świecie" | string toupper
```
Output: WITAJ ŚWIECIE

```Fish Shell
echo "TUtrzy słOwa" | string tolower
```
Output: tutrzy słowa

```Fish Shell
echo "mój naPiS" | string capitalize
```
Output: Mój naPiS

## Głębsze zagadnienia
W przypadku, kiedy chcesz zmienić jedynie kilka wybranych liter w napisie, możesz użyć funkcji `string replace`. Jest ona przydatna, gdy np. chcesz zamienić wszystkie spacje na znaki podkreślenia w celu utworzenia poprawnej nazwy pliku.

```Fish Shell
echo "To jest przykładowy napis" | string replace " " "_"
```
Output: To_jest_przykładowy_napis

Ponadto, jeśli chcesz bardziej zaawansowanej funkcjonalności, Fish Shell umożliwia również dostęp do wbudowanych narzędzi systemu operacyjnego. Można to wykorzystać do wywołania programów zewnętrznych, takich jak `tr` do zmiany wielkości liter.

## Zobacz również
- [Dokumentacja Funkcji String w Fish Shell](https://fishshell.com/docs/current/cmds/string.html)
- [Inne wbudowane narzędzia w Fish Shell](https://fishshell.com/docs/current/commands.html)
- [Przykłady wykorzystania funkcji string w Fish Shell](https://www.freecodecamp.org/news/string-manipulation-in-fish-shell-c34c33774e94/) (w języku angielskim)