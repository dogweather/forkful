---
title:                "Escribiendo pruebas"
date:                  2024-01-19
simple_title:         "Escribiendo pruebas"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/writing-tests.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir pruebas es crear código para verificar que tu código funcione como esperas. Los programadores las hacen para prevenir errores, hacer refactoring sin miedo y ayudar a otros a entender su uso.

## Cómo se hace:
Vamos a usar `unittest`, un framework de pruebas incluido en Python:

```Python
import unittest

def suma(x, y):
    return x + y

class PruebaSuma(unittest.TestCase):
    def test_suma(self):
        self.assertEqual(suma(3, 4), 7)

if __name__ == '__main__':
    unittest.main()
```

Ejecuta el script y verás algo como:

```
.
----------------------------------------------------------------------
Ran 1 test in 0.000s

OK
```

## Inmersión Profunda
Las pruebas son una parte vital del desarrollo desde hace décadas. Alternativas a `unittest` incluyen `pytest` y `nose`. Al escribir pruebas, considera casos borde y uso erróneo para hacer tu código más robusto.

## Ver También
- Documentación oficial de unittest: https://docs.python.org/3/library/unittest.html
- Pytest: https://docs.pytest.org/
- Historia del desarrollo guiado por pruebas (TDD): https://en.wikipedia.org/wiki/Test-driven_development
