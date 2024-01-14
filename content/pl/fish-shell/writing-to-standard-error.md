---
title:                "Fish Shell: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszym wpisie omówimy, dlaczego pisanie do standardowego błędu może być ważnym narzędziem w naszym programowaniu. Poznamy również kilka prostych przykładów, jak tego użyć w języku Fish Shell.

## Jak To Zrobić

Fish Shell oferuje nam prosty i przejrzysty sposób na przekierowanie informacji do standardowego błędu za pomocą operatora "2>". Przykładowy kod wyglądałby następująco:

```Fish Shell
ls -1 /home/bruno 2>errors.txt
```

W tym przykładzie używamy polecenia "ls" aby wyświetlić zawartość katalogu "/home/bruno", a za pomocą operatora "2>" przekierowujemy wszystkie błędy, które mogą wystąpić do pliku "errors.txt". 

Możemy też użyć tego narzędzia do wyświetlania konkretnych komunikatów, na przykład:

```Fish Shell
echo "Ups, coś poszło nie tak!" 2>/dev/null
```

Tutaj przekierowujemy komunikat "Ups, coś poszło nie tak!" do standardowego błędu, ale jednocześnie używamy operatora "/dev/null" aby ten błąd nie został wyświetlony na ekranie.

## Głębsze Zagłębienie

Pisanie do standardowego błędu może okazać się bardzo przydatne, gdy piszemy skrypty lub programy, które mają działać bez nadzoru użytkownika. Dzięki przekierowaniu błędów do pliku, możemy łatwiej monitorować nasz kod i w razie wystąpienia problemu szybko zareagować.

Pamiętajmy jednak, że zbyt duża ilość przekierowanych błędów może sprawić, że pliki będą szybko rosnąć i zabierać cenne miejsce na naszym dysku. Dlatego musimy uważnie dobierać jakie informacje chcemy przekierować do standardowego błędu.

## Zobacz też

- [Dokumentacja Fish Shell](https://fishshell.com/docs/current/)
- [Przekierowanie wyjścia i błędów w języku Fish Shell](https://devdungeon.com/content/stdout-stderr-redirect-fish-shell)