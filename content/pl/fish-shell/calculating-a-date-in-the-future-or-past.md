---
title:    "Fish Shell: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Dlaczego
Liczenie daty w przyszłości lub przeszłości może być niezbędne w wielu sytuacjach, na przykład przy planowaniu zadań lub wydarzeń lub przy obliczaniu wieku w przeszłości. W tym artykule pokażemy, jak można to zrobić za pomocą języka Fish Shell.

## Jak to zrobić
Liczenie daty w przyszłości lub przeszłości w Fish Shell jest bardzo proste. Wystarczy użyć wbudowanej funkcji `date`.

Najpierw należy ustawić aktualną datę przy użyciu `set` i `date`, a następnie dodać lub odjąć odpowiednią liczbę dni, tygodni, miesięcy lub lat. Na przykład, aby obliczyć datę za pół roku od dzisiaj, można użyć poniższego kodu:

```Fish Shell
set current_date (date)
set future_date (date -u +6mo)
echo $future_date
```

W tym przypadku użyłem flagi `-u`, która oznacza, że ​​wynikowa data będzie w formacie UTC. Można również użyć innych flag, aby dostosować format wyniku. Pełna lista dostępnych flag jest dostępna w dokumentacji Fish Shell.

## Deep Dive
Jeśli chcesz dowiedzieć się więcej o liczeniu daty w przyszłości lub przeszłości, warto skorzystać z funkcji `man` w Fish Shell. Wpisz po prostu `man date` w terminalu, aby przeczytać szczegółową dokumentację dotyczącą tej funkcji.

Mamy także dostęp do kilku innych funkcji, które mogą być przydatne przy pracy z datami w Fish Shell, takich jak `time`, `strftime` czy `strptime`. Można je również znaleźć w dokumentacji Fish Shell.

## Zobacz również
- [Dokumentacja Fish Shell](https://fishshell.com/docs/current/index.html)
- [Funkcja date w Fish Shell](https://fishshell.com/docs/current/cmds/date.html)
- [Poradnik Fish Shell na GitHub](https://github.com/fish-shell/fish-shell)