---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:41.383441-07:00
description: "Pobieranie bie\u017C\u0105cej daty w Pythonie to podstawowa operacja\
  \ dla wielu aplikacji, takich jak logowanie, analiza danych i podejmowanie decyzji\
  \ opartych na\u2026"
lastmod: 2024-02-19 22:04:54.144348
model: gpt-4-0125-preview
summary: "Pobieranie bie\u017C\u0105cej daty w Pythonie to podstawowa operacja dla\
  \ wielu aplikacji, takich jak logowanie, analiza danych i podejmowanie decyzji opartych\
  \ na\u2026"
title: Pobieranie aktualnej daty
---

{{< edit_this_page >}}

## Co i dlaczego?

Pobieranie bieżącej daty w Pythonie to podstawowa operacja dla wielu aplikacji, takich jak logowanie, analiza danych i podejmowanie decyzji opartych na czasie. Chodzi o uzyskanie bieżącej daty systemu, co jest kluczowe dla zadań zależnych od kontekstu czasowego.

## Jak to zrobić:

**Korzystając z biblioteki standardowej `datetime`:**

Moduł `datetime` w standardowej bibliotece Pythona dostarcza klasy do manipulowania datami i czasem. Aby uzyskać bieżącą datę, możesz użyć metody `date.today()`.

```python
from datetime import date

today = date.today()
print(today)  # Wynik: RRRR-MM-DD (np. 2023-04-05)
```

**Formatowanie czasu:**

Jeśli potrzebujesz bieżącej daty w innym formacie, metoda `strftime` pozwala określić niestandardowe formatowanie daty:

```python
from datetime import date

today = date.today()
formatted_date = today.strftime('%B %d, %Y')  # Przykładowy format: "Kwiecień 05, 2023"
print(formatted_date)
```

**Korzystanie z `pendulum` dla większej elastyczności (popularna biblioteka innych firm):**

`Pendulum` to biblioteka innych firm, która oferuje bardziej intuicyjne podejście do obsługi dat i czasu w Pythonie. Rozszerza ona standardowe funkcjonalności datetime i upraszcza zarządzanie strefami czasowymi, wśród innych funkcji.

Najpierw upewnij się, że zainstalowałeś `pendulum` za pomocą pip:

```shell
pip install pendulum
```

Następnie, aby uzyskać bieżącą datę:

```python
import pendulum

today = pendulum.now().date()
print(today)  # Wynik: RRRR-MM-DD (np. 2023-04-05)
```

Z `pendulum` formatowanie jest również proste i podobne do podejścia `strftime`:

```python
import pendulum

today = pendulum.now()
formatted_date = today.to_formatted_date_string()  # Domyślny format: "Kwi 5, 2023"
print(formatted_date)
```
