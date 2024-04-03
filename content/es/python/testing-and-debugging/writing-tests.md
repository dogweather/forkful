---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:27.244641-07:00
description: "Escribir pruebas en Python implica crear scripts automatizados para\
  \ validar la correcci\xF3n de tu c\xF3digo. Los programadores hacen esto para asegurarse\
  \ de\u2026"
lastmod: '2024-03-13T22:44:58.615922-06:00'
model: gpt-4-0125-preview
summary: "Escribir pruebas en Python implica crear scripts automatizados para validar\
  \ la correcci\xF3n de tu c\xF3digo."
title: Escribiendo pruebas
weight: 36
---

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
