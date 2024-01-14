---
title:    "Fish Shell: Konwersja daty na ciąg znaków"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą pracującym z językiem Fish Shell, prawdopodobnie musiałeś już zmienić format daty w postaci łańcucha znaków. Konwersja daty jest częstą czynnością w programowaniu i może być wyzwaniem dla początkujących lub nawet doświadczonych programistów. W tym wpisie przeczytasz jak w prosty sposób można wykonać konwersję daty na łańcuch znaków w języku Fish Shell.

## Jak to zrobić

Aby przekonwertować datę na łańcuch znaków w języku Fish Shell, możesz użyć funkcji `date` wraz z flagą `+%Y-%m-%d` aby określić format łańcucha wyjściowego. Spójrzmy na przykładowy kod:

```
Fish Shell - Konwersja daty na łańcuch znaków

#!/usr/bin/fish
set currentDate (date +%Y-%m-%d)
echo "Dzisiejsza data to: $currentDate"
```

W powyższym przykładzie, używamy funkcji `date` wraz z flagą `+%Y-%m-%d`, co oznacza, że chcemy uzyskać datę w formacie roku-miesiąc-dzień. Następnie używamy funkcji `echo`, aby wydrukować naszą datę wraz z tekstem "Dzisiejsza data to: ". Gdy uruchomimy ten skrypt, zobaczymy na ekranie naszą datę w formacie łańcucha znaków.

## Deep Dive

Istnieje wiele różnych opcji i flag, które możesz użyć w funkcji `date` w celu dostosowania formatu łańcucha wyjściowego do swoich potrzeb. Na przykład, flaga `+%A` zwróci nazwę dnia tygodnia, a flaga `+%H:%M:%S` zwróci godzinę, minutę i sekundę. Możesz także użyć znaków specjalnych, takich jak `\n` aby wstawiać nowe linie lub `\t` aby wstawić tabulację. Przykład ten pokazuje tylko podstawy konwersji daty na łańcuch znaków w języku Fish Shell, ale możliwości są znacznie większe.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o konwersji daty w języku Fish Shell, polecam przeczytać następujące artykuły:

- [Fish Shell - dokumentacja](https://fishshell.com/docs/current/index.html)
- [Jak konwertować daty w bashu](https://www.baeldung.com/linux/convert-date-to-string-bash)
- [Konwersja daty w języku Python](https://www.w3schools.com/python/python_datetime.asp)

Dziękujemy za przeczytanie naszego wpisu, mam nadzieję, że to pomoże wam w waszych przyszłych projektach!