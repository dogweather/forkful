---
title:    "Fish Shell: Konwersja daty na łańcuch znaków"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest ważnym aspektem programowania w środowisku Fish Shell. Jest to niezbędne przy tworzeniu skryptów i automatyzacji zadań, które wymagają obsługi dat.

## Jak to zrobić

Aby przekonwertować datę na ciąg znaków w Fish Shell, należy wykorzystać funkcję `date`. Poniżej przedstawione są różne sposoby użycia tej funkcji:

```Fish Shell
date +%Y-%m-%d                   # 2021-10-28
date +%d.%m.%Y                   # 28.10.2021
date +"Jest %A, %d %B %Y roku"   # Jest czwartek, 28 października 2021 roku
```

Możliwości formatowania są nieograniczone, dlatego warto zapoznać się z dokumentacją Fish Shell, aby poznać wszystkie dostępne opcje.

## Dogłębne zagłębienie

Konwersja daty na ciąg znaków może być również połączona z innymi funkcjami Fish Shell, takimi jak `for` lub `while`, aby przetwarzać wiele dat jednocześnie. Przykładowo, można utworzyć skrypt, który wyświetli daty z ostatnich 7 dni i przekonwertuje je na format DD.MM.RRRR:

```Fish Shell
for x in (seq 1 7)
    set day (date -v-$x'd' +%d.%m.%Y)
    echo $day
end
```

Powyższy kod utworzy następujący output:

```Fish Shell
21.10.2021
22.10.2021
23.10.2021
24.10.2021
25.10.2021
26.10.2021
27.10.2021
```

## Zobacz również

- Oficjalna dokumentacja Fish Shell: [https://fishshell.com/docs/current/cmds/date.html](https://fishshell.com/docs/current/cmds/date.html)
- Przewodnik po Fish Shell: [https://fishshell.com/docs/current/tutorial.html](https://fishshell.com/docs/current/tutorial.html)
- Źródło: [https://www.linuxjournal.com/content/format-date-shell-script](https://www.linuxjournal.com/content/format-date-shell-script)