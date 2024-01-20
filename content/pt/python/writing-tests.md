---
title:                "Escrevendo testes"
html_title:           "Python: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/writing-tests.md"
---

{{< edit_this_page >}}

## O que e Por que?

Escrever testes é uma prática comum entre programadores, pois ajuda a garantir que o código que eles escreveram funcione corretamente. Ao escrever testes, os programadores podem garantir que seu código está cumprindo todas as especificações e que não há erros em seu funcionamento. Isso economiza tempo e esforço no longo prazo, tornando o processo de desenvolvimento mais eficiente.

## Como fazer:

Escrever testes em Python é simples e pode ser feito com a ajuda de algumas bibliotecas úteis, como o Unittest e o Pytest. Um exemplo básico seria criar uma classe de teste que herda da classe TestCase do Unittest e, em seguida, definir alguns métodos para testar funções específicas. Em seguida, executar o teste garantirá que todas as funções estão funcionando como deveriam. Aqui está um exemplo de código:

```Python
import unittest

def add_two_numbers(x, y):
    return x + y

class TestFunctions(unittest.TestCase):
    def test_add_two_numbers(self):
        self.assertEqual(add_two_numbers(2, 3), 5)
        self.assertEqual(add_two_numbers(5, -2), 3)

if __name__ == '__main__':
    unittest.main()
```

A saída desse teste seria:

```
.
----------------------------------------------------------------------
Ran 1 test in 0.000s

OK
```

O ponto no topo significa que o teste passou com sucesso.

## Aprofundamento:

A prática de escrever testes tem suas raízes na metodologia ágil e no desenvolvimento orientado a testes. Isso significa que os testes são escritos antes do código e são usados para orientar o processo de desenvolvimento. Além disso, existem outras bibliotecas de teste disponíveis para uso em Python, como o Robot Framework e o Behave. Essas bibliotecas fornecem recursos adicionais para testes de interface do usuário e testes de aceitação.

## Veja também:

- [Documentação oficial do Unittest](https://docs.python.org/3/library/unittest.html)
- [Documentação oficial do Pytest](https://docs.pytest.org/en/latest/)