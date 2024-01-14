---
title:                "Bash: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest niezbędnym elementem w wielu skryptach Bash. Wiele systemów operacyjnych i innych narzędzi używa danych w formacie tekstowym, dlatego jest ważne umieć przetwarzać datę w taki sposób.

## Jak to zrobić

Zaczniemy od prostego przykładu, w którym konwertujemy obecną datę na ciąg znaków. W Bash możemy wykorzystać polecenie `date` oraz opcję `+%m/%d/%Y`, która ustala format wyjściowy na miesiąc/dzień/rok.

```Bash
# Przykładowy skrypt 
current_date=$(date '+%m/%d/%Y')
echo "Dzisiaj jest $current_date"
```

Wynikiem wywołania tego skryptu będzie "Dzisiaj jest 06/26/2021". Naturalnie, możemy zmienić format daty według własnych preferencji, używając innych opcji dostępnych w poleceniu `date`, takich jak `+%A` dla pełnego nazwy dnia tygodnia lub `+%H:%M` dla godziny i minut.

## Pogłębione zagadnienia

Podstawowa konwersja daty jest prosta i przydatna w codziennych skryptach, ale istnieją także inne możliwości. Bash oferuje wiele wbudowanych funkcji, takich jak `strftime`, która pozwala na bardziej zaawansowane manipulowanie datą i czasem. Ta funkcja pozwala na dostosowywanie formatu wyjściowego do własnych potrzeb, wykorzystując specjalne znaki, np. `%m` dla miesiąca w postaci liczby lub `%b` dla nazwy miesiąca w postaci skróconej.

```Bash
# Przykład z wykorzystaniem strftime 
current_date=$(strftime "%M %b %Y" $(date +"%s"))
echo "To jest $current_date"
```

W wyniku powyższego skryptu otrzymamy "To jest 14 Jun 2021". Kombinując ze specjalnymi znakami i korzystając z wbudowanych funkcji, możemy uzyskać dokładnie taki format daty, jaki potrzebujemy.

## Zobacz także

- [Dokumentacja polecenia date w Bash](https://www.gnu.org/software/coreutils/date)
- [Kompletny przewodnik po konwersji daty w Bash](https://blog.learningtree.com/pl/konwersja-daty-w-bash/)
- [Skrypty daty i czasu w Bash](https://www.baeldung.com/linux/bash-date-commands)