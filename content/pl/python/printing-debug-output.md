---
title:                "Python: Drukowanie wyników debugowania"
simple_title:         "Drukowanie wyników debugowania"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Wyświetlanie debugowania jest ważnym narzędziem dla każdego programisty. W ten sposób możesz sprawdzić, co dzieje się w Twoim kodzie i jakie wartości są przetwarzane. Może pomóc w zidentyfikowaniu błędów i usprawnieniu kodu.

## Jak to zrobić

Możesz wyświetlać komunikaty debugowania w Pythonie za pomocą funkcji print(). Możesz też użyć specjalnego poziomu logowania, aby wyświetlić tylko niektóre informacje w zależności od potrzeb.

```Python
# Przykład użycia funkcji print()
print("Wartość x wynosi:", x)

# Przykład użycia poziomu logowania 
import logging
logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger(__name__)
logger.debug("Wartość x wynosi: %s", x)
```

Kiedy używasz funkcji print(), pamiętaj o umieszczeniu odpowiednich informacji, takich jak wartości zmiennych czy komentarze, aby ułatwić sobie debugowanie w przyszłości.

## Głębsze zagłębienie

Jeśli chcesz jeszcze głębiej poznać możliwości wyświetlania debugowania w Pythonie, możesz zacząć od zapoznania się z pakietem pdb (Python Debugger). Pozwala on na interaktywne debugowanie kodu, w które możesz się zagłębić i sprawdzić jego działanie krok po kroku.

Możesz również wykorzystać funkcję assert, aby upewnić się, że Twoje zmienne mają poprawne wartości, a w przeciwnym razie wyświetlić komunikat błędu.

## Zobacz też

- [Dokumentacja funkcji print() w Pythonie](https://docs.python.org/3/library/functions.html#print)
- [Dokumentacja modułu logging w Pythonie](https://docs.python.org/3/library/logging.html)
- [Dokumentacja modułu pdb w Pythonie](https://docs.python.org/3/library/pdb.html)
- [Pomoc w debugowaniu kodu w Pythonie](https://realpython.com/python-debugging-pdb/)
- [Jak wykorzystać assert do testowania i debugowania kodu w Pythonie](https://www.blog.pythonlibrary.org/2016/09/10/python-201-asserting-your-assumptions/)