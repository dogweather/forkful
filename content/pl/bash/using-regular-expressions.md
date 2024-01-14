---
title:    "Bash: Używanie wyrażeń regularnych"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Regularne wyrażenia to niezbędne narzędzie dla każdego programisty, który chce efektywnie przetwarzać tekst. Pozwala ono na wyszukiwanie, zastępowanie i manipulowanie tekstem w sposób precyzyjny i szybki. Dzięki regularnym wyrażeniom możemy również sprawdzać poprawność danych wejściowych, co jest niezwykle ważne w programowaniu.

## Jak to zrobić

Aby zacząć korzystać z regularnych wyrażeń w Bashu, musimy najpierw znać podstawy składni. Zwykle wygląda ona tak: `wyrażenie_regularne` `plik_tekstowy` - czyli najpierw podajemy wzorzec, a następnie plik, w którym chcemy go przeszukać. Przykładowo, jeśli chcemy znaleźć wszystkie numery telefonów w pliku o nazwie `kontakty.txt`, możemy użyć takiego wyrażenia: `(\+48)*[0-9]{9}`. Wówczas wyświetlą się wszystkie numery zaczynające się od polskiego kodu kraju (+48) oraz zawierające 9 cyfr.

Poniżej przedstawiam kilka przykładów zastosowań regularnych wyrażeń w Bashu:

```
Bash ```

#### Wyszukanie wszystkich słów zaczynających się na "A" w pliku `tekst.txt`:

```
grep "^A" tekst.txt
```

#### Zastąpienie wszystkich wystąpień "python" na "Bash" w pliku `projekt.py`:

```
sed -i 's/python/Bash/g' projekt.py
```

#### Sprawdzenie poprawności adresu email w zmiennej `email`:

```
if [[ "$email" =~ ^[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,}$ ]]; then
    echo "Poprawny adres email."
fi
```

## Głębszy zanurzenie

Regularne wyrażenia mogą być nieco skomplikowane na początku, ale z praktyką i dobrą znajomością składni, będą one nieocenionym narzędziem w naszym arsenałe programisty. Warto również wykorzystywać inne komendy i narzędzia, takie jak `grep`, `sed` czy `awk` w połączeniu z wyrażeniami regularnymi, aby uzyskać jeszcze większą kontrolę nad przetwarzaniem tekstu.

## Zobacz również

- [Dokumentacja Bash o regularnych wyrażeniach](https://www.gnu.org/software/bash/manual/html_node/Pattern-Matching.html)
- [Kurs regularnych wyrażeń w Bashu na Codecademy](https://www.codecademy.com/learn/learn-regex)
- [10 przykładów użycia regularnych wyrażeń w Bashu](https://www.geeksforgeeks.org/10-best-practices-vscode-regex/)