---
title:                "Escrevendo testes"
date:                  2024-01-19
html_title:           "Arduino: Escrevendo testes"
simple_title:         "Escrevendo testes"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/writing-tests.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Testes são scripts que automaticamente verificam se um código faz o que deve. Programadores testam para prevenir bugs, economizar tempo e manter a qualidade.

## Como Fazer:

Vamos usar o `unittest` – um módulo de testes padrão do Python.

```python
import unittest

def soma(a, b):
    return a + b

class TesteDaSoma(unittest.TestCase):
    def test_soma_numeros_positivos(self):
        self.assertEqual(soma(2, 3), 5)
    
    def test_soma_numeros_negativos(self):
        self.assertEqual(soma(-1, -1), -2)

if __name__ == '__main__':
    unittest.main()
```

Rode o teste e veja:

```
..
----------------------------------------------------------------------
Ran 2 tests in 0.001s

OK
```

## Aprofundando:

O `unittest` foi inspirado no JUnit e tem uma abordagem similar. Alternativas incluem o PyTest, com menos cerimônia, e o Nose, que é extensível. Os testes devem cobrir casos típicos e exceções, automatizando a verificação de que tudo funciona após mudanças.

## Veja Também:

- Documentação oficial do `unittest`: https://docs.python.org/3/library/unittest.html
- PyTest para um estilo mais simplificado: https://docs.pytest.org/en/stable/
- Nose para um framework de teste mais extensível: https://nose.readthedocs.io/en/latest/
