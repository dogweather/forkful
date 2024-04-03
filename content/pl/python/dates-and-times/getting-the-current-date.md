---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:41.383441-07:00
description: "Jak to zrobi\u0107: **Korzystaj\u0105c z biblioteki standardowej `datetime`:**\
  \ Modu\u0142 `datetime` w standardowej bibliotece Pythona dostarcza klasy do manipulowania\u2026"
lastmod: '2024-03-13T22:44:34.960500-06:00'
model: gpt-4-0125-preview
summary: "**Korzystaj\u0105c z biblioteki standardowej `datetime`:**\n\nModu\u0142\
  \ `datetime` w standardowej bibliotece Pythona dostarcza klasy do manipulowania\
  \ datami i czasem."
title: Pobieranie aktualnej daty
weight: 29
---

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
