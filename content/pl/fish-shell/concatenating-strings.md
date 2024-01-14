---
title:    "Fish Shell: Łączenie ciągów znaków"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Zarówno dla nowych, jak i doświadczonych programistów, operowanie na łańcuchach znaków jest nieodłączną częścią codziennej pracy. Jest to jedna z najważniejszych umiejętności, ponieważ pozwala na łączenie różnych fragmentów tekstu w jedną spójną całość. W tym artykule dowiesz się, dlaczego warto poznać technikę łączenia stringów w języku Fish Shell.

## Jak to zrobić

Aby połączyć dwa lub więcej łańcuchów znaków w języku Fish Shell, wystarczy użyć operatora plus (+) lub polecenia string bult-in join. Poniżej znajdziesz przykładowy kod oraz wynik działania:

```Fish Shell
set first_name "Jan"
set last_name "Kowalski"

echo $first_name" "$last_name
```
Wynik: "Jan Kowalski"

```Fish Shell
string join -s " " $first_name $last_name
```
Wynik: "Jan Kowalski"

Jak widać, można łączyć nie tylko stałe ciągi znaków, ale także zmienne przechowujące tekst. W przypadku użycia operatora plus należy pamiętać o dodaniu spacji pomiędzy łańcuchami, aby połączone słowa nie tworzyły jednego wyrazu.

## Głębsze zagadnienia

Poza dodawaniem spacji, możliwe jest także wykorzystanie innych separatorów w poleceniu string join. Wystarczy zmienić wartość opcji -s, na przykład:

```Fish Shell
string join -s ", " "jabłka" "banany" "gruszki"
```
Wynik: "jabłka, banany, gruszki"

Dodatkowo, w języku Fish Shell można również łączyć łańcuchy znaków z innymi wartościami, takimi jak liczby czy listy. Połączone ciągi będą automatycznie konwertowane w odpowiedni sposób. Przykłady:

```Fish Shell
set age 25

echo "Mam " $age " lat."
```
Wynik: "Mam 25 lat."

```Fish Shell
set fruits (jabłka banany gruszki)

echo "Lubię jeść: " (string join ", " $fruits)
```
Wynik: "Lubię jeść: jabłka, banany, gruszki"

## Zobacz także

- Dokumentacja języka Fish Shell: https://fishshell.com/docs/current/index.html
- Przewodnik po podstawowych operacjach na stringach: https://fishshell.com/docs/current/tutorial.html#tutorial-strings
- Wprowadzenie do konstrukcji języka Fish Shell: https://medium.com/@eemeliheinonen/fish-is-a-strongly-typed-language-85c76cfbcb26