---
title:                "Bash: Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Obliczanie daty w przeszłości lub przyszłości może być przydatne w wielu przypadkach, na przykład gdy chcesz ustawić przypomnienie lub zaprogramować automatyczne zadania. Jest to również przydatne w celu wykonywania obliczeń dat w skryptach Bash.

## Jak to zrobić

```Bash
# Przykład obliczenia daty 7 dni w przód
future_date=$(date -d "+7 days" +%m/%d/%Y)
echo $future_date
# Output: 06/15/2021

# Przykład obliczenia daty 30 dni wstecz
past_date=$(date -d "-30 days" +%m/%d/%Y)
echo $past_date
# Output: 04/17/2021
```

## Pogłębiona analiza

Bash posiada wbudowany pakiet `date`, który pozwala na manipulowanie datami. Można użyć opcji `-d` w celu ustalenia przyszłej lub przeszłej daty. Można również użyć różnych opcji formatowania, aby uzyskać żądaną formę daty. Należy pamiętać, że niektóre dystrybucje Linux mogą różnić się w sposobie obsługi komendy `date`, więc należy sprawdzić dokumentację systemu, aby upewnić się, że używasz odpowiednich opcji.

## Zobacz również

- [Dokumentacja komendy date w Bash](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Manipulowanie datami w Bash](https://www.baeldung.com/linux/bash-date-command)
- [Formatowanie dat w Bash](https://www.cyberciti.biz/faq/howto-get-current-date-time-in-bash-linux-shell-script/)