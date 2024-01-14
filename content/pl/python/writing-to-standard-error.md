---
title:    "Python: Pisanie do standardowego błędu"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego standardowe wyjście jest ważne?

 W programowaniu w języku Python istnieje kilka metod wypisywania komunikatów dla użytkownika. Jedną z nich jest standardowe wyjście, czyli po prostu wyświetlenie tekstu w konsoli. Ale co w przypadku błędów? Dlaczego warto wypisywać je do standardowego wyjścia błędów? Czy to nie lepiej utworzyć osobny plik z logami? W tym artykule postaram się odpowiedzieć na te pytania i pokazać przykłady użycia standardowego wyjścia błędów.

## Jak to zrobić?

Aby wypisać komunikat do standardowego wyjścia błędów w Pythonie, używamy funkcji `print()` z parametrem `file=sys.stderr`. Przykładowo:

```Python
import sys
print("To jest błąd", file=sys.stderr)
```

Ten komunikat zostanie wypisany do konsoli jako błąd, a nie jako zwykły tekst. Możemy również wykorzystać ten sam sposób do obsługi wyjątków:

```Python
try:
    x = 5/0
except ZeroDivisionError as e:
    print("Nastąpiło dzielenie przez zero!", file=sys.stderr)
    print("Komunikat błędu:", e, file=sys.stderr)
```

W powyższym przykładzie, komunikat o błędzie zostanie wyświetlony wraz z informacją o rodzaju błędu.

W przypadku pisania większych projektów, przydatne może być również zapisywanie błędów do pliku logów, aby móc je później przeanalizować. Wtedy można wykorzystać moduł `logging` i ustawić go, aby zapisywał błędy do pliku. Przykładowo:

```Python
import logging
logging.basicConfig(level=logging.ERROR,
                    filename='error.log',
                    format='%(asctime)s - %(levelname)s - %(message)s')

try:
    x = 5/0
except ZeroDivisionError as e:
    logging.error("Nastąpiło dzielenie przez zero!", exc_info=True)
```

W tym przypadku, błąd zostanie zapisany do pliku `error.log` wraz z datą, poziomem i treścią. Możemy również ustawić inny poziom logowania, w zależności od potrzeb.

## Głębsza analiza

Wypisywanie błędów do standardowego wyjścia pozwala nam na szybkie zauważenie problematycznych miejsc w kodzie. Pomaga również w debugowaniu aplikacji, ponieważ wszelkie błędy zostaną od razu wyświetlone. Natomiast zapisywanie ich do pliku logów daje nam możliwość późniejszej analizy i znalezienia przyczyn błędów.

Warto również pamiętać o ustawianiu odpowiednich poziomów logowania w zależności od rodzaju błędu. Dzięki temu można skutecznie filtrować informacje i szybciej znaleźć właściwe miejsce w kodzie.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o obsłudze błędów i logowaniu w Pythonie, polecam zapoznać się z poniższymi artykułami:

- [Obsługa błędów w Pythonie](https://www.python.org/dev/peps/pep-3109/)
- [Dokumentacja modułu `logging` w Pythonie](https://docs.python.org/3/library/logging.html)

Dzięki tym materiałom będziesz mógł jeszcze lepiej wykorzystać możliwości standardowego wyjścia błędów i zwiększyć jakość swojego kodu.