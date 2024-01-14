---
title:                "Fish Shell: Wydrukowanie wyników debugowania"
programming_language: "Fish Shell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Drukowanie informacji debuggowania jest istotnym elementem programowania w języku Fish Shell. Pozwala ono na sprawdzanie aktualnego stanu programu oraz debugowanie potencjalnych błędów w kodzie. Jest to szczególnie przydatne przy tworzeniu skomplikowanych skryptów lub aplikacji.

## Jak to zrobić

Aby wydrukować informacje debuggowania w języku Fish Shell, należy użyć funkcji `echo`. Przykładowy kod:

```Fish Shell
set name "Jan"
echo "Witaj, $name!"
```

Output:

```
Witaj, Jan!
```

Możemy również wykorzystać flagę `-d` do wyświetlenia informacji debuggowania w czasie wykonywania skryptu. Przykładowy kod:

```Fish Shell
set age 25
set name "Anna"
echo -d "Witaj, $name! Masz $age lat."
```

Output:

```
Job: echo -d Witaj, $name! Masz $age lat.
    Spec variable bindings:
        age=25
        name=Anna
Witaj, Anna! Masz 25 lat.
```

## Głębszy zanurkowanie

W języku Fish Shell możemy również wykorzystać funkcję `printf` do drukowania informacji debuggowania w bardziej wyrafinowany sposób. Przykładowy kod:

```Fish Shell
set x 10
set y 5
printf "Wartość zmiennej x: %d, wartość zmiennej y: %d\n" $x $y
```

Output:

```
Wartość zmiennej x: 10, wartość zmiennej y: 5
```

Funkcja `printf` pozwala nam również na formatowanie wyjścia, co może być przydatne podczas debugowania bardziej złożonego kodu.

## Zobacz również

- [Oficjalna dokumentacja języka Fish Shell](https://fishshell.com/docs/current/index.html)
- [Przewodnik po języku Fish Shell](https://devhints.io/fish)
- [Bezpłatny kurs programowania w języku Fish Shell](https://fish-tutorial.com/)