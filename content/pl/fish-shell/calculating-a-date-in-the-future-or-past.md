---
title:    "Fish Shell: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Będąc programistą, często musimy pracować z datami w przyszłości lub przeszłości. Odliczanie dni, tygodni lub miesięcy może być czasochłonne i trudne. Jednak dzięki kodowaniu w Fish Shell, możemy szybko i łatwo obliczyć datę w przyszłości lub przeszłości.

## Jak to zrobić?

Aby obliczyć datę w przyszłości lub przeszłości w Fish Shell, musimy użyć wbudowanej komendy "date" z flagą "-d". Przykładowa składnia kodu wygląda następująco:

```
Fish Shell> date -d "1 day"
```
To spowoduje wyświetlenie daty za 1 dzień w przyszłości. Innym przydatnym poleceniem jest "date -d", aby obliczyć datę w przeszłości. Na przykład:

```
Fish Shell> date -d "1 week ago"
```

Powyższy kod wyświetli datę z tygodniem wstecz w przeszłości. Możemy również używać różnych jednostek czasu, takich jak miesiące i lata. Na przykład:

```
Fish Shell> date -d "2 months"
```

Spowoduje wyświetlenie daty za 2 miesiące w przyszłości. Aby zobaczyć pełną listę dostępnych opcji dla komendy "date -d", możemy użyć polecenia "man date" w Fish Shell.

## Dogłębnie

Obliczanie daty w przyszłości lub przeszłości jest możliwe dzięki wbudowanej komendzie "date" w Fish Shell, która używa biblioteki GNU Date. Ta biblioteka oferuje wiele opcji, które pozwalają nam na obliczanie dat w różnych jednostkach czasu i wyświetlanie wyniku w różnych formatach. W Fish Shell możemy również używać zmiennych, aby przechowywać obliczone daty i wykorzystać je w naszym kodzie.

## Zobacz również

- [Dokumentacja Fish Shell dotycząca obliczania daty](https://fishshell.com/docs/current/commands.html#date)
- [Dokumentacja GNU Date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)