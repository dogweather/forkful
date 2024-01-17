---
title:                "Imprimir saída de depuração"
html_title:           "Python: Imprimir saída de depuração"
simple_title:         "Imprimir saída de depuração"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## O que e por que?

Printar a saída de depuração é basicamente mostrar informações adicionais no terminal durante a execução de um programa. Os programadores utilizam isso para verificar o valor de variáveis, entender o fluxo de execução e solucionar erros.

## Como fazer:

Para imprimir a saída de depuração em Python, utilize a função `print()` e passe como argumento a informação que deseja visualizar. Por exemplo:
```Python
x = 5
print(x) # imprime o valor de x
```
Isso irá imprimir `5` no terminal. Além disso, também é possível utilizar a função `format()` para imprimir informações mais detalhadas, como por exemplo:
```Python
nome = "Maria"
idade = 25
print("Olá, meu nome é {} e tenho {} anos.".format(nome, idade))
```
Que irá imprimir `Olá, meu nome é Maria e tenho 25 anos.`

## Aprofundando:

A impressão de saída de depuração já existe há muito tempo e é uma técnica amplamente utilizada por programadores para identificar e solucionar problemas em seus códigos. Existem outras maneiras de depurar, como utilizar um depurador integrado ao ambiente de desenvolvimento, mas muitas vezes a impressão de saída de depuração é mais simples e eficaz.

Para implementar a impressão de saída de depuração de forma mais sofisticada, é possível utilizar a biblioteca `logging` do Python, que oferece recursos avançados de registro de mensagens. Além disso, também é comum adicionar identificadores às mensagens de depuração, como `DEBUG` ou `ERROR`, para facilitar sua leitura e organização.

## Veja também:

- [Documentação oficial do Python sobre a função print()](https://docs.python.org/3/library/functions.html#print)
- [Documentação oficial do Python sobre a biblioteca logging](https://docs.python.org/3/library/logging.html)
- [Artigo sobre como imprimir saída de depuração em Python](https://tryolabs.com/blog/2015/12/15/pythonic-ways-to-use-python-print/)