---
title:                "Bash: Uzyskiwanie bieżącej daty"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu potrzebujemy informacji o dacie. Może to być potrzebne do wygenerowania raportu, zapisania pliku z datą w nazwie lub do wyświetlenia aktualnego czasu dla użytkownika. W tym artykule dowiesz się, w jaki sposób w prosty sposób uzyskać aktualną datę w języku Bash, aby łatwo móc jej używać w swoich programach.

## Jak to zrobić

Istnieje kilka sposobów na uzyskanie aktualnej daty w Bash. Następujące przykłady przedstawiają najprostszą metodę, ale istnieje wiele innych, które mogą być dostępne w twoim systemie operacyjnym lub wersji Bash. Możesz wypróbować różne metody, aby znaleźć tę, która najlepiej spełnia twoje potrzeby.

```Bash
# Dostępna data i godzina
date

# Aktualna data
date +"%F"

# Aktualny czas
date +"%T"

# Format 12-godzinny z AM/PM
date +"%r"

# Aktualny rok
date +"%Y"

# Aktualny miesiąc
date +"%m"

# Aktualny dzień
date +"%d"
```

### Wysoki poziom

Komenda `date` jest wykorzystywana do uzyskania informacji o dacie i czasie. Może przyjąć różne formaty i opcje, co daje duże możliwości w dostosowaniu uzyskiwanych danych do swoich potrzeb. Możesz wyświetlić pomoc w konsoli, wpisując `man date` lub `date --help`, aby poznać dostępne opcje i ich szczegółowe opisy.

Jeśli potrzebujesz użyć aktualnej daty w nazwie pliku, np. do utworzenia kopii zapasowej lub zapisania pliku z unikalną nazwą, warto skorzystać z komendy `touch` lub `cp` i wprowadzić odpowiednio sformatowany wynik uzyskany z `date`. Przykładowo, jeśli chcesz utworzyć plik z datą i godziną w nazwie, możesz użyć następującej komendy:

```Bash
# Utworzenie pliku z datą i godziną w nazwie
touch file_$(date +"%F_%T").txt
```

## Zagłębienie

Polecam dokładnie przeanalizować formatowanie komendy `date`, ponieważ dostępne jest wiele opcji, które mogą ułatwić korzystanie z aktualnej daty. Warto także poeksperymentować z różnymi opcjami, aby lepiej zrozumieć, jak działają. W ten sposób będziesz bardziej pewny w użyciu jej w swoich programach.

## Zobacz także

- Dokumentacja komendy `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html#date-invocation
- Przydatne opcje komendy `date`: https://www.codecoffee.com/tipsforlinux/articles/35.html
- Poradnik na temat pracy z datami w Bash: https://www.lifewire.com/use-command-to-work-with-dates-2201058