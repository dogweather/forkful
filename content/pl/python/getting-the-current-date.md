---
title:                "Python: Pobieranie aktualnej daty"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie kodu jest niesamowicie ważną częścią nauki tworzenia oprogramowania. Jednak czasami można zauważyć, że trudno jest znaleźć konkretne praktyczne przykłady, które pozwolą nam zrozumieć tę umiejętność w odniesieniu do codziennych zadań. Często używamy gotowych funkcji bez zastanowienia się nad tym, jak działają i dlaczego są nam potrzebne. W dzisiejszym poście zajmiemy się tematem pobierania daty w języku Python i wyjaśnimy, dlaczego jest to ważne oraz jak to zrobić.

## Jak to zrobić

Pobieranie aktualnej daty jest ważnym aspektem w wielu projektach programistycznych. Jest to przydatne do celów raportowania, ściągania danych z określonego okresu czasu czy też do zachowania porządku w plikach zapisywanych w określonych dniach. W języku Python istnieje wiele sposobów na pobranie aktualnej daty. Najprostszy z nich to użycie modułu `datetime`, który jest dostępny w standardowej bibliotece języka. Aby pobrać aktualną datę, użyjemy funkcji `date.today()` z tego modułu. Poniżej zamieszczamy przykładowy kod i jego wynik:

```python
from datetime import date

today = date.today()
print(today)
```
_# wynik: 2021-01-01_

Jeśli chcemy wyświetlić datę w innym formacie, możemy użyć metody `strftime()` i przekazać mu odpowiedni format jako argument. Przykład takiego użycia prezentuje poniższy kod:

```python
from datetime import date

today = date.today()
print(today.strftime("%d-%m-%Y"))
```
_# wynik: 01-01-2021_

W powyższych przykładach użyliśmy funkcji `date.today()`, jednak istnieje również możliwość pobrania aktualnego czasu za pomocą funkcji `datetime.datetime.now()`, która zwraca obiekt zawierający zarówno datę, jak i czas. Poniżej przedstawiamy przykładowy kod i jego wynik:

```python
from datetime import datetime

now = datetime.now()
print(now)
```
_# wynik: 2021-01-01 12:34:56.789_

Tworzenie obiektu `datetime` pozwala także na dostęp do poszczególnych elementów daty i czasu, takich jak dzień, miesiąc, rok czy też godzina, minuty i sekundy. Przykładowo, aby wyświetlić tylko Godzinę i Minutę, możemy użyć poniższego kodu:

```python
from datetime import datetime

now = datetime.now()
print(now.strftime("%H:%M"))
```
_# wynik: 12:34_

## Deep Dive

Podczas pobierania aktualnego czasu w języku Python warto zwrócić uwagę na różnice między czasem lokalnym, a uniwersalnym czasem koordynowanym (UTC). W przypadku funkcji `datetime.now()` zwracany jest czas lokalny, co może być przydatne dla użytkowników, którzy korzystają z programów w różnych strefach czasowych. Jednak w niektórych przypadkach warto przekonwertować czas lokalny na UTC lub odwrotnie, na przykład przy synchronizacji danych zapisanych w różnych strefach czasowych.

## Zobacz także

- [Dokumentacja biblioteki datetime w języku Python](https://docs.python.org/3/library/datetime.html)
- [Przetwarzanie dat i czasu w języku Python](https://realpython.com/python-datetime/)
- [Tutorial o czasie w języku Python](https://www.programiz.com/python-programming/datetime)