---
title:    "Bash: Odczytywanie argumentów wiersza poleceń"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Cześć! Jeśli jesteś początkującym programistą lub po prostu chcesz lepiej zrozumieć język Bash, to dobrze trafiłeś. W tym artykule dowiesz się, dlaczego warto nauczyć się czytać argumenty w wierszu poleceń (command line arguments) oraz nauczysz się, jak to zrobić. Czytaj dalej, jeśli jesteś zainteresowany rozwijaniem swoich umiejętności w języku Bash!

## Jak to zrobić

Czasami podczas uruchamiania skryptu w Bash, możesz chcieć przekazać pewne informacje lub parametry, które będą wpływać na działanie programu. Tutaj właśnie przychodzą nam z pomocą argumenty w wierszu poleceń. Aby odczytać te argumenty, musimy użyć zmiennej "$@", która zawiera wszystkie przekazane argumenty w formie listy. Przykładowy kod wyglądałby następująco:

```Bash
#!/bin/bash

# Przykładowy skrypt czytający argumenty w wierszu poleceń

echo "Hej, podaj swoje imię: "

read name  # wczytujemy imię ze standardowego wejścia

# "$@" pozwala nam na przekazanie wszystkich argumentów jako argument listy
echo "Witaj, $name ! Twoje argumenty to: $@"

# przykład uruchomienia: ./skrypt.sh argument1 argument2
# Wyjście: Witaj, Adam ! Twoje argumenty to: argument1 argument2
```

Jak możesz zauważyć, zmienne "$@" są wykorzystywane do wyświetlenia wszystkich przekazanych argumentów. Możemy także odwołać się do konkretnych argumentów poprzez indeksowanie listy (np. "$1" dla pierwszego argumentu, "$2" dla drugiego itp.). Możliwości jest wiele, więc po prostu eksperymentuj i odkrywaj!

## Deep Dive

Jeśli chcesz dowiedzieć się więcej na temat czytania argumentów w wierszu poleceń, istnieje wiele dodatkowych zasobów, które mogą Ci pomóc. Możesz przeczytać dokumentację języka Bash lub poszukać różnych tutoriali i przykładów w Internecie. Zapewniam Cię, że znajdziesz mnóstwo przydatnych informacji i zastosowań dla tej umiejętności.

## Zobacz także

- [Dokumentacja języka Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Tutorial na temat czytania argumentów w wierszu poleceń](https://www.shellscript.sh/arguments.html)
- [Blog Bash hackers](https://www.bash-hackers.org/wiki/doku.php)