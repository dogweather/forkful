---
title:                "Bash: Drukowanie wyników debugowania"
programming_language: "Bash"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli już próbowałeś programować w Bash, być może zauważyłeś, że jednym z najważniejszych narzędzi do debugowania kodu jest wypisywanie wyjścia. W tym artykule dowiesz się, dlaczego drukowanie komunikatów debugowania jest kluczowym elementem w pracy z Bash i jak możesz skutecznie wykorzystać to narzędzie w swoim kodzie.

## Jak to zrobić

Aby wypisać wyjście debugowania w Bash, możesz użyć komendy `echo` z odpowiednim flagiem. Na przykład, aby wyświetlić wartość zmiennej `i`, możesz użyć poniższego kodu:

```bash
echo "Wartość zmiennej i to: $i"
```

Będzie to wypisywać na ekranie "Wartość zmiennej i to: [wartość zmiennej i]". Możesz również wykorzystać operator `>` aby przekierować to wyjście do pliku. Na przykład:

```bash
echo "Wartość zmiennej i to: $i" > output.txt
```

Powyższy kod zapisze wartość zmiennej `i` do pliku `output.txt`. Istnieje również opcja `>>`, która będzie dodawać nowe zawartości do istniejącego pliku, zamiast go nadpisywać.

## Głębszy zanurzenie

Drukowanie wyjścia debugowania jest kluczowym narzędziem w procesie debugowania kodu w Bash. Pozwala to na śledzenie wartości zmiennych i wyświetlanie komunikatów diagnostycznych w różnych etapach wykonywania kodu. Może to pomóc w łatwiejszym zlokalizowaniu i rozwiązaniu błędów w kodzie.

Jedną z ciekawszych opcji jest użycie kolorów w wyjściu debugowania. Można to osiągnąć poprzez użycie polecenia `tput setaf` z odpowiednimi parametrami. Na przykład, aby ustawić kolor na czerwony, można użyć:

```bash
echo "$(tput setaf 1) Error: Wystąpił błąd $(tput sgr0)"
```

Zobaczysz, że wyjście będzie wyświetlać tekst "Error: Wystąpił błąd" w kolorze czerwonym. To jest tylko jedna z wielu opcji, które można wykorzystać do bardziej czytelnego i przejrzystego wyjścia debugowania.

## Zobacz również

- [Dokumentacja Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Kurs Bash na Codecademy](https://www.codecademy.com/learn/learn-bash)