---
title:    "Bash: Konwertowanie daty na ciąg znaków"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwertowanie daty na łańcuch znaków jest często potrzebne w programowaniu, zwłaszcza w Bashu. Pozwala na wygodniejszą pracy z datami i wykorzystanie ich w różnych operacjach, takich jak wyświetlanie, porównywanie czy zapisywanie do pliku.

## Jak to zrobić

Konwersja daty w Bashu jest bardzo prosta i wygodna. Wystarczy użyć polecenia `date` w połączeniu z odpowiednimi opcjami. Poniżej przedstawiamy przykłady użycia dla różnych formatów daty.

```Bash
# Konwersja aktualnej daty na łańcuch znaków w formacie RRRR-MM-DD

date +"%Y-%m-%d"
# output: 2021-08-23

# Konwersja aktualnej godziny na łańcuch znaków w formacie GG:MM:SS

date +"%H:%M:%S"
# output: 14:30:54

# Konwersja aktualnego dnia tygodnia na łańcuch znaków w formacie DDD (skrócona nazwa)

date +"%a"
# output: Mon

# Konwersja aktualnego dnia tygodnia na łańcuch znaków w formacie DDDD (pełna nazwa)

date +"%A"
# output: Monday
```

W powyższych przykładach użyto opcji `+%` do określenia formatu, w jakim ma być zwrócona data lub godzina. Można dowolnie dobierać opcje, aby dostosować format do swoich potrzeb.

## Wnikliwsze omówienie

W Bashu mamy do dyspozycji wiele opcji dla polecenia `date`, które pozwalają na dokładne ustawienie formatu zwracanej daty. Możemy wybrać pełną nazwę dnia tygodnia, skróconą lub nawet liczbową wartość miesiąca. Możemy również wybrać format daty z uwzględnieniem czasu, strefy czasowej czy formatu litery graficznej.

Możemy także używać poleceń warunkowych, takich jak `if/else`, aby określić specjalne przypadki konwersji daty. Przykładowo, jeśli chcemy zwrócić łańcuch znaków "Dzisiaj jest sobota", gdy aktualna data jest sobotą, a inny łańcuch znaków w pozostałe dni tygodnia, możemy użyć poniższego kodu:

```Bash
if [ $(date +"%a") == "Sat" ]
then
  echo "Dzisiaj jest sobota"
else
  echo "Dzisiaj jest $(date +%A)"
fi
```

Takie wykorzystanie konwersji daty na łańcuch znaków pozwala na bardziej elastyczne i czytelne programowanie w Bashu.

## Zobacz również

- [Dokumentacja polecenia `date` w Bashu](https://ss64.com/bash/date.html)
- [Przydatne przykłady konwersji daty w Bashu](https://www.baeldung.com/linux/bash-date-command)