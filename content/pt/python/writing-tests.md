---
title:    "Python: Escrevendo testes"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/python/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes é importante?

Escrever testes é uma parte crucial do processo de programação em Python. Testes garantem que o código funcione corretamente e evitam que bugs sejam introduzidos no sistema. Eles também facilitam a identificação e correção de erros, economizando tempo e esforço no longo prazo.

## Como escrever testes em Python

Para escrever testes em Python, é importante seguir algumas etapas básicas. Primeiro, importe o módulo `unittest` para utilizar suas funções de teste. Em seguida, crie uma classe para o seu teste usando a sintaxe `class TestXXXX(unittest.TestCase)`. Dentro da classe, escreva métodos de teste usando a sintaxe `def test_func():` seguida de asserções para verificar se o resultado está correto. Por fim, execute os testes usando o método `unittest.main()`.

Um exemplo de código de teste pode ser visto abaixo:

```Python
import unittest

class TestCalculadora(unittest.TestCase):
    def test_soma(self):
        resultado = 2+2
        self.assertEqual(resultado, 4)

    def test_subtracao(self):
        resultado = 10-5
        self.assertEqual(resultado, 5)

    def test_multiplicacao(self):
        resultado = 3*4
        self.assertEqual(resultado, 12)

if __name__ == '__main__':
    unittest.main()
```

O código acima cria uma classe `TestCalculadora` e três métodos de teste para verificar as operações de soma, subtração e multiplicação. Ao executar o arquivo, o resultado será `OK`, indicando que os testes passaram.

## Adentrando mais profundamente no tema

Existem diferentes tipos de testes que podem ser escritos em Python, como testes unitários, testes de integração e testes de aceitação. Cada tipo de teste tem seu propósito específico e é importante entender essas diferenças para garantir uma cobertura adequada dos testes. Além disso, existem ferramentas como o `coverage` que podem ser usadas para verificar a cobertura dos seus testes.

Também é importante lembrar que testes devem ser escritos de forma independente e não devem depender de outros testes para funcionar corretamente. Eles também devem ser atualizados conforme o código muda, garantindo que os testes ainda sejam relevantes e precisos.

## Veja também

- [Python.org: Unittest](https://docs.python.org/3/library/unittest.html)
- [Python.org: Cobertura de testes](https://coverage.readthedocs.io/en/coverage-5.5/)
- [Testes unitários vs testes de integração vs testes de aceitação](https://medium.com/meritt/testes-unit%C3%A1rios-vs-testes-de-integra%C3%A7%C3%A3o-vs-testes-de-aceita%C3%A7%C3%A3o-de061a4e8d5e)