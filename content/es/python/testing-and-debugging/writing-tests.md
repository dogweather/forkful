---
title:                "Escribiendo pruebas"
aliases: - /es/python/writing-tests.md
date:                  2024-02-03T19:31:27.244641-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escribiendo pruebas"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Escribir pruebas en Python implica crear scripts automatizados para validar la corrección de tu código. Los programadores hacen esto para asegurarse de que sus funciones o clases funcionen como se espera bajo varias condiciones, lo que ayuda a detectar errores temprano y facilita un mantenimiento y refactorización más sencillos.

## Cómo hacerlo:
Python viene con un módulo incorporado para escribir pruebas llamado `unittest`. Así es como puedes usarlo para probar una función simple:

```python
import unittest

def add(a, b):
    return a + b

class TestAddFunction(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add(1, 2), 3)
        self.assertEqual(add(-1, 1), 0)
        self.assertNotEqual(add(10, 2), 12, "Debería ser 12")

if __name__ == '__main__':
    unittest.main()
```

Cuando ejecutes este script de prueba, deberías ver una salida indicando que tus pruebas pasaron (o fallaron).

Para pruebas más modernas y expresivas, puedes usar una biblioteca de terceros como `pytest`. Primero, tendrás que instalarla usando pip:

```shell
pip install pytest
```

Luego, puedes escribir tus pruebas de manera más simple sin necesidad de subclasificar nada:

```python
# Guarda esto en un archivo llamado test_with_pytest.py
def add(a, b):
    return a + b

def test_add():
    assert add(1, 2) == 3
    assert add(-1, 1) == 0
    assert add(10, 2) != 12, "Debería ser 12"
```

Para ejecutar tus pruebas con `pytest`, simplemente ejecuta:

```shell
pytest test_with_pytest.py
```

Deberías ver la salida de pytest mostrando los resultados de tus pruebas.
