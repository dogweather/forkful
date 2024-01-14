---
title:                "Python: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes é importante no desenvolvimento de software?

Escrever testes é uma prática essencial no desenvolvimento de software. Os testes garantem que o código está funcionando corretamente e previnem erros futuros. Além disso, eles fornecem uma documentação viva do código e ajudam a identificar problemas mais rapidamente durante o processo de desenvolvimento.

## Como escrever testes em Python

Para começar a escrever testes em Python, é necessário utilizar a biblioteca de testes integrada no Python, chamada "unittest". Para isso, basta importá-la no topo do seu arquivo:

```Python
import unittest
```

Em seguida, crie a classe de teste que irá conter seus métodos de teste:

```Python
class MeuTeste(unittest.TestCase):
```

Dentro da classe, você pode escrever os métodos de teste utilizando o prefixo "test_" antes do nome do método. Por exemplo:

```Python
def test_soma(self):
    resultado = 2 + 2
    self.assertEqual(resultado, 4)
```

Este método "test_soma" irá garantir que a soma de 2 + 2 é igual a 4. Para executar os testes, basta chamar o método "main" da biblioteca unittest:

```Python
if __name__ == '__main__':
    unittest.main()
```

Isso irá rodar todos os métodos de teste dentro da classe "MeuTeste". O resultado dos testes será exibido no terminal, como por exemplo:

```
----------------------------------------------------------------------
Ran 1 test in 0.000s

OK
```

## Mergulho Profundo em Testes

Existem diferentes tipos de testes que podem ser escritos em Python, como testes unitários, testes de integração e testes funcionais. Cada um deles tem uma finalidade específica no processo de desenvolvimento de software. Além disso, é possível utilizar ferramentas de cobertura de código, como o "coverage", que ajudam a medir a eficiência dos seus testes.

Também é importante seguir algumas boas práticas ao escrever testes em Python. Isso inclui manter os testes independentes e coesos, e utilizar asserções adequadas para verificar as funcionalidades do código.

## Veja também

- [Documentação oficial de testes do Python](https://docs.python.org/3/library/unittest.html)
- [Tutorial de testes de Python pela Real Python](https://realpython.com/python-testing/)
- [Guia de boas práticas para escrever testes em Python](https://medium.com/@vladyslavhoncharenko/7-best-practices-for-writing-javascript-and-python-unit-tests-1c0cb19c1b63)

- [Ferramenta de cobertura de código "coverage"](https://coverage.readthedocs.io/en/stable/)