---
title:    "Python: Uzyskiwanie bieżącej daty"
keywords: ["Python"]
---

{{< edit_this_page >}}

# Dlaczego pobieranie aktualnej daty jest ważne?

Pobieranie aktualnej daty jest ważne z wielu powodów. Przede wszystkim, wiele aplikacji i skryptów wymaga dostępu do aktualnej daty w celu wykonywania pewnych operacji, na przykład generowania raportów lub logów. Ponadto, często potrzebujemy aktualnej daty do celów organizacyjnych, aby śledzić terminy lub określać ważne wydarzenia.

# Jak to zrobić?

W Pythonie pobranie aktualnej daty jest bardzo proste. Wystarczy użyć wbudowanego modułu "datetime" i wywołać metodę "now()", która zwróci aktualną datę i czas. Poniżej przedstawiony jest przykładowy kod oraz jego wynik:

```python
import datetime

aktualna_data = datetime.datetime.now()
print(aktualna_data)
```

Wynik:
2020-10-14 13:57:44.841708

Możemy również sformatować datę według własnych preferencji, używając metody "strftime()". Przykładowy kod i jego wynik:

```python
import datetime

aktualna_data = datetime.datetime.now()
sformatowana_data = aktualna_data.strftime("%d-%m-%Y")
print(sformatowana_data)
```

Wynik:
14-10-2020

# Głębsze zanurzenie

Moduł "datetime" oferuje wiele innych możliwości manipulacji datą i czasem. Możemy na przykład dodawać lub odejmować dni, tygodnie, miesiące lub lata z daty, sprawdzać, który dzień tygodnia jest danego dnia, czy też ustawiać własne formaty daty. Więcej informacji na temat tego modułu można znaleźć w oficjalnej dokumentacji Pythona.

# Zobacz również

- Oficjalna dokumentacja Pythona: https://docs.python.org/pl/3/library/datetime.html
- Poradnik "datetime" w programowaniu w Pythonie: https://realpython.com/python-datetime/
- Przewodnik po obsłudze dat i czasów w Pythonie: https://www.dataquest.io/blog/python-datetime-tutorial/