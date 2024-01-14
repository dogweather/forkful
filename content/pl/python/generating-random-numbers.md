---
title:    "Python: Generowanie losowych liczb"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego?

Generowanie losowych liczb jest ważną częścią nauki programowania. Pozwala ono na tworzenie różnorodnych aplikacji oraz może być używane jako narzędzie do testowania kodu.

## Jak to zrobić?

Do generowania losowych liczb w Pythonie można użyć wbudowanej funkcji `random`. Poniżej znajdują się przykłady kodów i wyników dla różnych metod generacji losowych liczb:

```Python
# Generowanie całkowitych liczb losowych
import random
random_number = random.randint(1, 10)
print(random_number)
# Output: 7 
```

```Python
# Generowanie liczb zmiennoprzecinkowych z zakresu 0-1
import random
random_float = random.random()
print(random_float) 
# Output: 0.75863569143
```

```Python
# Generowanie losowej wartości z listy
import random
numbers = [1, 5, 10, 20, 100]
random_number = random.choice(numbers)
print(random_number) 
# Output: 5 
```

## Pogłębione wyjaśnienie

Generowanie losowych liczb w komputerze jest trudne, ponieważ komputery są zaprogramowane do działania w sposób przewidywalny. Dlatego też, do wygenerowania liczb losowych używa się różnych algorytmów, które wykorzystują różnorodne źródła danych, takie jak pomiary czasu czy ruch myszy. Jest to tzw. "pseudo-losowość", gdyż wynik jest w rzeczywistości określony przez określony algorytm. Jednakże, w większości przypadków, pseudo-losowość jest wystarczająco dobra dla większości zastosowań.

## Zobacz także

- Dokumentacja Pythona na temat modułu `random`: [https://docs.python.org/3/library/random.html](https://docs.python.org/3/library/random.html)
- Wprowadzenie do generowania  losowych liczb w Pythonie: [https://realpython.com/python-random/](https://realpython.com/python-random/)
- Tutorial na temat generatorów liczb losowych i programowania funkcyjnego w Pythonie: [https://realpython.com/primer-on-python-decorators/](https://realpython.com/primer-on-python-decorators/)
- Przykładowe aplikacje wykorzystujące losowe liczby w Pythonie: [https://www.pythonforbeginners.com/code-snippets-source-code/python-code-examples-using-lists](https://www.pythonforbeginners.com/code-snippets-source-code/python-code-examples-using-lists)