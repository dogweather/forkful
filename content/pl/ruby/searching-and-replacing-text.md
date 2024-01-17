---
title:                "Wyszukiwanie i wymienianie tekstu"
html_title:           "Ruby: Wyszukiwanie i wymienianie tekstu"
simple_title:         "Wyszukiwanie i wymienianie tekstu"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Co & Dlaczego?
Szukanie i zamiana tekstu jest kluczowym elementem w pracy programisty. Pozwala nam w szybki i prosty sposób zmienić określone fragmenty tekstu w naszym kodzie. Dzięki temu możemy uniknąć ręcznego edytowania wszystkich wystąpień danego tekstu, co jest czasochłonne i może prowadzić do błędów. 

# Jak to zrobić:
Możemy używać wbudowanych metod w Ruby do wyszukiwania i zamiany tekstu. Oto kilka przykładowych funkcji:

```Ruby
# Zamiana wystąpienia jednego słowa na inne
str = "Cześć, jestem Ruby"
puts str.gsub("Ruby", "programista")
# Output: Cześć, jestem programista

# Zamiana kilku słów na inne
str = "Koduję w Ruby, ale czasami używam też Pythona"
puts str.gsub("Ruby", "Ruby on Rails").gsub("Pythona", "Django")
# Output: Koduję w Ruby on Rails, ale czasami używam też Django
```

Możemy również użyć wyrażeń regularnych, aby dokonać bardziej szczegółowych zmian. Na przykład, aby zamienić wszystkie liczby w tekście na ich kwadraty, możemy użyć takiej funkcji:

```Ruby
str = "Te słowa mają odpowiednio 1, 2 i 3 literki"
puts str.gsub(/\d+/) { |match| (match.to_i)**2 }
# Output: Te słowa mają odpowiednio 1, 4 i 9 literki
```
## Głęboki Zanurzenie:
Szukanie i zamiana tekstu już od samego początku było istotną częścią programowania. Wcześniej, programiści musieli dokonywać tych zmian ręcznie, co było bardzo czasochłonne. Jednak obecnie, dzięki zaawansowanym narzędziom, możemy to robić w kilka prostych krokach.

Alternatywnym sposobem na dokonywanie zmian w tekście jest użycie narzędzi do edycji tekstu takich jak Vim, Emacs lub Sublime Text. 

Podczas zamiany tekstu, ważne jest również aby pamiętać o występujących różnicach w wielkości liter. Dlatego używanie metod takich jak `gsub` (global substitution) jest zalecane, ponieważ dokonują one zmian bez względu na wielkość liter. 

## Zobacz też:
Jeśli chcesz dowiedzieć się więcej na temat wyszukiwania i zamiany tekstu w Ruby, zalecam przeczytanie dokumentacji Ruby lub znalezienie tutoriala online. 

- [Dokumentacja Ruby](https://www.ruby-lang.org/pl/documentation/)
- [Dokumentacja Ruby - metoda gsub](https://ruby-doc.org/core-2.6.3/String.html#method-i-gsub)
- [Kurs Ruby w Codecademy](https://www.codecademy.com/learn/learn-ruby)