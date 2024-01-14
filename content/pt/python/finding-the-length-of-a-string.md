---
title:    "Python: Encontrando o comprimento de uma string"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Porquê

Em programação, é muito comum termos que lidar com cadeias de caracteres ou strings. E uma tarefa bastante comum é a de encontrar o comprimento de uma string, ou seja, quantos caracteres ela possui. Saber como fazer isso é fundamental para diversas aplicações, desde a validação de senha até a manipulação de dados em bancos de dados.

## Como Fazer

Para encontrar o comprimento de uma string em Python, utilizamos a função nativa `len()`. Essa função recebe como parâmetro a string desejada e retorna o seu comprimento como um número inteiro. Veja um exemplo:

```Python
nome = "João"
comprimento = len(nome)

print(comprimento)
```
**Saída: 4**

Também é possível utilizar a função `len()` em conjunto com a função `input()`, permitindo que o usuário insira a string desejada. Veja como ficaria o código:

```Python
nome = input("Digite o seu nome: ")
comprimento = len(nome)

print("O seu nome possui", comprimento, "caracteres.")
```
**Saída: O seu nome possui (número de caracteres) caracteres.**

## Mergulho Profundo

É importante destacar que, em Python, a função `len()` também pode ser utilizada em outros tipos de dados, além de strings. Ela pode ser utilizada, por exemplo, em listas, dicionários e tuplas.

Outro ponto importante é que a função `len()` conta todos os caracteres dentro da string, incluindo espaços em branco e caracteres especiais. Além disso, ela também conta os caracteres de escape, como `\n` e `\t`.

## Veja Também

- [Documentação oficial da função `len()`](https://docs.python.org/3/library/functions.html#len)
- [Como manipular strings em Python](https://www.python.org/dev/peps/pep-0257/#manipulating-docstrings)
- [Validação de senha em Python](https://pynative.com/python-generate-random-string/)