---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:31.846122-07:00
description: "Escrever testes em Python envolve a cria\xE7\xE3o de scripts automatizados\
  \ para validar a corre\xE7\xE3o do seu c\xF3digo. Programadores fazem isso para\
  \ garantir que\u2026"
lastmod: '2024-03-13T22:44:46.157511-06:00'
model: gpt-4-0125-preview
summary: "Escrever testes em Python envolve a cria\xE7\xE3o de scripts automatizados\
  \ para validar a corre\xE7\xE3o do seu c\xF3digo."
title: Escrevendo testes
weight: 36
---

## O Que & Por Que?
Escrever testes em Python envolve a criação de scripts automatizados para validar a correção do seu código. Programadores fazem isso para garantir que suas funções ou classes funcionem conforme esperado sob várias condições, o que ajuda a capturar erros precocemente e facilita a manutenção e refatoração mais fáceis.

## Como fazer:
Python vem com um módulo embutido para escrever testes chamado `unittest`. É assim que você pode usá-lo para testar uma função simples:

```python
import unittest

def add(a, b):
    return a + b

class TestAddFunction(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add(1, 2), 3)
        self.assertEqual(add(-1, 1), 0)
        self.assertNotEqual(add(10, 2), 12, "Deveria ser 12")

if __name__ == '__main__':
    unittest.main()
```

Quando você executa esse script de teste, você deve ver uma saída indicando que seus testes passaram (ou falharam).

Para testes mais modernos e expressivos, você pode usar uma biblioteca de terceiros como `pytest`. Primeiro, você terá que instalá-la usando o pip:

```shell
pip install pytest
```

Depois, você pode escrever seus testes de uma maneira mais simples, sem a necessidade de subclasse:

```python
# Salve isso em um arquivo chamado test_with_pytest.py
def add(a, b):
    return a + b

def test_add():
    assert add(1, 2) == 3
    assert add(-1, 1) == 0
    assert add(10, 2) != 12, "Deveria ser 12"
```

Para executar seus testes com `pytest`, simplesmente execute:

```shell
pytest test_with_pytest.py
```

Você deve ver a saída do pytest mostrando os resultados dos seus testes.
