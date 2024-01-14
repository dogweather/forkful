---
title:    "Python: Pobieranie aktualnej daty"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu potrzebujemy aktualnej daty. Może to być potrzebne do tworzenia plików, zapisywania danych lub wyświetlania informacji dla użytkownika. W tym artykule dowiesz się, jak w prosty sposób pobrać aktualną datę w języku Python.

## Jak to zrobić

Aby pobrać aktualną datę w języku Python, możemy skorzystać z modułu "datetime". W pierwszym kroku musimy zaimportować ten moduł do naszego programu. Korzystając z funkcji "now()" i "date()" możemy pobrać odpowiednio bieżącą datę i czas lub tylko datę.

```Python 
import datetime

# bieżąca data i czas
current_datetime = datetime.now()
print(current_datetime)

# tylko data
current_date = datetime.date()
print(current_date)
```

**Output:**

2021-10-19 13:22:34.125490
2021-10-19

Możemy również sformatować datę w dowolny sposób, korzystając z funkcji "strftime()". Dzięki temu możemy wyświetlić datę w wybranym przez nas formacie, na przykład w formacie "RRRR-MM-DD".

```Python
import datetime

current_date = datetime.date()
formatted_date = current_date.strftime("%Y-%m-%d")

print(formatted_date)
```

**Output:**

2021-10-19

## Wnikliwsze spojrzenie

W module "datetime" dostępne są również inne funkcje i metody, które pozwalają na manipulowanie datami. Na przykład możemy wyświetlić dzień tygodnia lub dodawać/odejmować dni od bieżącej daty.

```Python
import datetime

# wyświetlenie dnia tygodnia (poniedziałek = 0, niedziela = 6)
week_day = datetime.date().weekday()
print(week_day)

# dodawanie 7 dni do bieżącej daty
new_date = datetime.date() + datetime.timedelta(days=7)
print(new_date)
```

**Output:**

1
2021-10-26

Warto również pamiętać o różnicach w datach i czasie, na przykład podczas pracy z różnymi strefami czasowymi. Możemy stosować różne metody, takie jak "utcnow()" lub "astimezone()" w celu dostosowania do naszych potrzeb.

## Zobacz także

- Dokumentacja modułu datetime: https://docs.python.org/3/library/datetime.html
- Poradnik wideo dotyczący pracy z datami w Pythonie: https://www.youtube.com/watch?v=XKHEtdqhLK8
- Przykłady z wykorzystaniem modułu datetime: https://www.programiz.com/python-programming/datetime