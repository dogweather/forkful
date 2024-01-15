---
title:                "Imprimindo saída de depuração"
html_title:           "Python: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que

Algumas vezes, durante a programação, pode ser necessário entender o processo e o fluxo de execução do código. Ao imprimir saídas de depuração (debug output), podemos visualizar o que está acontecendo dentro do nosso código e identificar possíveis erros ou problemas.

## Como Fazer

Para imprimir saídas de depuração em Python, utilizamos a função built-in `print()`. Vamos ver um exemplo simples, em que imprimimos o valor de uma variável:

```Python
nome = "Maria"
print("O nome é: ", nome)
```

A saída será `O nome é: Maria`, e podemos ver que a variável `nome` possui o valor "Maria". Além disso, podemos também imprimir múltiplos valores separando-os por vírgula dentro da função `print()`:

```Python
a = 10
b = 5
print("A é igual a", a, "e B é igual a", b)
```

A saída será `A é igual a 10 e B é igual a 5`. Com isso, podemos monitorar o valor de diversas variáveis em diferentes partes do nosso código.

## Deep Dive

Além de imprimir valores de variáveis, também podemos utilizar a função `print()` para imprimir mensagens de depuração. Isso pode ser feito utilizando a formatação de strings em Python, especificando `%s` ou `%d` para indicar o valor de uma string ou um inteiro, respectivamente. Um exemplo:

```Python
numero = 10
print("O valor da variável número é %d" % numero) # %d é substituído pelo valor de numero
```

A saída será `O valor da variável número é 10`. Podemos ainda utilizar a função `format()` para formatar variáveis dentro de uma string, o que pode ser especialmente útil quando se trabalha com mais de um tipo de dado. Exemplo:

```Python
nome = "João"
idade = 25
print("Meu nome é {0} e eu tenho {1} anos".format(nome, idade))
```

A saída será `Meu nome é João e eu tenho 25 anos`.

## Veja Também

- [Documentação Python - F-string](https://docs.python.org/pt-br/3.8/library/string.html#formatstrings)
- [Artigo sobre Debugging em Python](https://realpython.com/python-debugging-pdb/)