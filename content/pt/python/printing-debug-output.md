---
title:                "Python: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saída de depuração é importante

Quando escrevemos um código complexo, é muito comum encontrar erros e bugs no processo. A saída de depuração é uma ferramenta essencial que nos ajuda a entender como o código está funcionando e onde estão os possíveis problemas. Sem ela, seria muito mais difícil encontrar e corrigir esses erros.

## Como imprimir saída de depuração

Abaixo, mostramos um exemplo simples de como imprimir uma mensagem de depuração em um código Python:

```Python
x = 5
y = 10

# Imprimir valores das variáveis
print("O valor de x é:", x)
print("O valor de y é:", y)

# Imprimir resultado da soma entre x e y
print("A soma entre x e y é:", x + y)
```

Saída:

```
O valor de x é: 5
O valor de y é: 10
A soma entre x e y é: 15
```

Podemos ver que ao imprimir as mensagens de depuração, conseguimos visualizar os valores das variáveis e o resultado da operação, o que nos ajuda a entender como o código está funcionando. Essa é apenas uma maneira simples de imprimir saída de depuração, mas existem outras formas mais avançadas de utilizá-la.

## Aprofundando na impressão de saída de depuração

Além de imprimir mensagens simples, também podemos utilizar a função `print()` para mostrar informações mais detalhadas, como por exemplo o tipo de dado de uma variável ou o valor de um determinado índice em uma lista. Podemos também utilizar a biblioteca `logging` para criar logs de depuração mais complexos e personalizáveis.

É importante saber quando e como usar a saída de depuração, pois ela pode ser muito útil na identificação e correção de erros em um código. Porém, é necessário ter cuidado para não utilizar excessivamente, pois isso pode diminuir a performance do programa.

## Veja também

- [Documentação oficial do Python](https://docs.python.org/3.9/tutorial/index.html)
- [Artigo sobre boas práticas de depuração em Python](https://realpython.com/python-debugging-pdb/#dont-use-print)
- [Explicação sobre a biblioteca logging](https://realpython.com/python-logging/)