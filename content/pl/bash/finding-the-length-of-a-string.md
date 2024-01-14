---
title:                "Bash: Szukanie długości ciągu znaków"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego 

W programowaniu Bash, często musimy operować na ciągach tekstu. W niektórych przypadkach, potrzebujemy również poznać długość tych ciągów. W tym artykule dowiesz się, dlaczego znajdowanie długości ciągów jest ważnym aspektem programowania Bash.

## Jak to zrobić

Aby znaleźć długość jednego ciągu tekstu w Bash, możemy skorzystać z wbudowanej funkcji `length`. Spójrz na poniższy przykład:

```Bash
string="To jest przykładowy ciąg tekstu"
echo "Długość ciągu to: ${#string}"
```

Powinniśmy otrzymać następujący wynik:

```
Długość ciągu to: 29
```

W powyższym przykładzie użyliśmy operatora `#` przed nazwą zmiennej, aby wyświetlić jej długość. Funkcja `length` jest równoznaczna z użyciem tego operatora.

Możemy również znaleźć długość wielu ciągów tekstu jednocześnie. Należy jednak pamiętać, aby oddzielać każdy ciąg spacją. Przykładowo:

```Bash
ciąg1="To jest pierwszy ciąg"
ciąg2="A to drugi ciąg"
ciąg3="I to ostatni ciąg"
echo "Długość wszystkich ciągów to: ${#ciąg1} ${#ciąg2} ${#ciąg3}"
```

Spodziewany wynik:

```
Długość wszystkich ciągów to: 21 17 19
```

## Dogłębna Analiza

Podczas znalezienia długości ciągów tekstu, warto zwrócić uwagę na kilka istotnych szczegółów. Po pierwsze, funkcja `length` zwraca również długość spacji. Jeśli więc chcemy wykluczyć spacje z obliczeń, musimy użyć funkcji `echo` z opcją `-n`:

```Bash
ciąg="To jest ciąg złożony z kilku wyrazów"
echo -n "Długość ciągu bez spacji: ${#ciąg}"
```

Wynik:

```
Długość ciągu bez spacji: 28
```

Kolejną ważną rzeczą do zapamiętania jest to, że funkcja `length` działa tylko na jedno-wymiarowych ciągach. Nie zadziała np. na tablicy lub wielowymiarowej zmiennej. W takim przypadku, musimy najpierw wyciągnąć odpowiednią część zmiennej, a następnie użyć funkcji `length`. Przykładowo:

```Bash
tablica=(element1 element2 element3)
echo "Liczba elementów w tablicy: ${#tablica[@]}"

tablica_2d=( [0]="a" [1]="b" [2]="c" [3]="d" )
echo "Liczba elementów: ${#tablica_2d[@]}" # Błędna wartość!

echo "Liczba elementów w pierwszym wierszu: ${#tablica_2d[0]}" # Odpowiednia wartość
```

Spodziewane wyniki:

```
Liczba elementów w tablicy: 3
Liczba elementów: 1 # Błędna wartość, bo liczy tylko pierwszy indeks (0)
Liczba elementów w pierwszym wierszu: 4 # Odpowiednia wartość
```

## Zobacz też 

- [Dokumentacja Bash o długości ciągów](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [Tutorial o zmiennej w Bash](https://linuxize.com/post/bash-variable-expansion/)
- [Wideo o wykorzystaniu funkcji `length`](https://www.youtube.com/watch?v=RUrooC3_P6