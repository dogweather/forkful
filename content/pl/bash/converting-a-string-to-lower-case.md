---
title:                "Bash: Konwertowanie ciągu znaków na małe litery"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

W tym wpisie na blogu dowiesz się, dlaczego i w jaki sposób warto przekonwertować ciąg znaków na małe litery w języku programowania Bash.

## Jak to zrobić

Konwersja ciągu znaków na małe litery w Bash jest bardzo prosta i wykorzystuje się do tego funkcję wbudowaną "tr". W poniższych przykładach wyjaśnimy krok po kroku jak tego dokonać.

```Bash
# Przykładowy ciąg znaków
string="PRZYKŁADOWY TEKST"

# Wyświetlenie oryginalnego ciągu
echo $string
```

Wynik:
```
PRZYKŁADOWY TEKST
```

```Bash
# Konwersja na małe litery
lower=$(echo $string | tr '[:upper:]' '[:lower:]')

# Wyświetlenie przekonwertowanego ciągu
echo $lower
```

Wynik:
```
przykładowy tekst
```

## Zagłębienie się w temat

Konwersja ciągu znaków na małe litery może być przydatna w wielu przypadkach. Na przykład podczas przetwarzania danych wejściowych, skanowania plików lub tworzenia nowych plików o jednolitym formatowaniu. Funkcja "tr" w Bash pozwala również na dodanie lub usunięcie innych znaków, jak również na zmianę wielkości liter.

Podczas konwersji na małe litery, "tr" zwraca nowy ciąg znaków, więc warto przypisać go do zmiennej, aby móc go wykorzystać w dalszej części skryptu.

## Zobacz również

- [Dokumentacja Bash](https://www.gnu.org/software/bash/manual/html_node/index.html)
- [Przydatne funkcje w Bash](https://linuxconfig.org/bash-functions)
- [Przekierowanie i przetwarzanie danych w Bash](https://www.shell-tips.com/bash/redirect-stdout-and-stderr/)