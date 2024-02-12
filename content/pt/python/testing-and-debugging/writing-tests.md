---
title:                "Escrevendo testes"
aliases:
- pt/python/writing-tests.md
date:                  2024-02-03T19:31:31.846122-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escrevendo testes"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
