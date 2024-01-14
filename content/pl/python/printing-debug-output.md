---
title:    "Python: Drukowanie wyjścia debugowania"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego drukować informacje debugujące

Jeśli pracujesz z językiem Python, na pewno spotkałeś się z sytuacją, w której twój kod nie działa poprawnie. W tym przypadku, jednym z najprostszych sposobów na znalezienie błędu jest wyświetlenie informacji debugujących. W tym artykule dowiesz się dlaczego warto drukować takie informacje oraz jak to zrobić.

## Jak to zrobić

Aby wyświetlić informacje debugujące w Pythonie, możesz skorzystać z funkcji `print()`. Przykładowo, w poniższym kodzie chcemy przetestować funkcję liczącą średnią z listy liczb.

```Python
numbers = [1, 2, 3, 4, 5]
avg = sum(numbers) / len(numbers)
print("Średnia to: ", avg)
```

Po uruchomieniu powyższego kodu, zobaczymy na ekranie następujący wynik:

```
Średnia to: 3.0
```

Dzięki wyświetleniu informacji debugujących, mamy pewność, że liczenie średniej zostało wykonane poprawnie. Możemy także wyświetlić inne zmienne czy informacje, które pomogą nam zlokalizować błąd w kodzie.

## Prześwietlenie

Drukowanie informacji debugujących jest szczególnie przydatne w przypadku bardziej skomplikowanych programów. Dzięki temu możemy zobaczyć, jak zmieniają się zmienne w różnych częściach kodu oraz dowiedzieć się, które linie są wykonywane.

W niektórych przypadkach, możemy także skorzystać z modułu `logging`, który daje możliwość dodatkowej konfiguracji wyświetlanych informacji debugujących. Jest to szczególnie przydatne w większych projektach, gdzie chcemy mieć bardziej przejrzysty i kontrolowany sposób wyświetlania tych informacji.

## Zobacz także

- [Dokumentacja Pythona na temat drukowania informacji debugujących](https://docs.python.org/3/library/functions.html#print)
- [Poradnik na temat użycia modułu logging](https://realpython.com/python-logging/)
- [Inne przydatne metody debugowania w Pythonie](https://realpython.com/python-debugging-pdb/)