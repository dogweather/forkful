---
title:                "Python: Generowanie losowych liczb"
programming_language: "Python"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Dlaczego

Generowanie losowych liczb jest nieodłączną częścią programowania w języku Python i jest niezwykle przydatne w różnych zastosowaniach. Może być wykorzystywane do symulowania przypadkowych zdarzeń, generowania losowych danych i testowania algorytmów.

# Jak to zrobić?

Do generowania losowych liczb w języku Python można użyć modułu `random`. Przykładowy kod wyglądałby następująco:

```Python
import random
print(random.randint(1, 10))
```

Powyższy kod wygeneruje losową liczbę całkowitą z zakresu od 1 do 10 i wyświetli ją na ekranie. Moduł `random` oferuje również wiele innych funkcji do generowania różnych rodzajów liczb.

# Głębsze wniknięcie

Generowanie liczb losowych jest w rzeczywistości procesem deterministycznym, który wykorzystuje tzw. generator liczb pseudolosowych. Generator ten korzysta z tzw. ziarna (ang. seed), czyli początkowej wartości, do generowania kolejnych liczb. Jeśli podamy takie samo ziarno, to wynik generowania liczb będzie taki sam. Dlatego też w niektórych przypadkach należy uważać na to, jakie ziarno jest wykorzystywane, aby uniknąć tzw. efektu deterministycznego.

# Zobacz też

- Dokumentacja modułu `random`: https://docs.python.org/3/library/random.html
- Przydatne porady dotyczące generowania liczb losowych w Pythonie: https://www.digitalocean.com/community/tutorials/how-to-use-the-random-module-in-python-3
- Wprowadzenie do generatorów liczb pseudolosowych: https://www.geeksforgeeks.org/pseudo-random-number-generator-prng/