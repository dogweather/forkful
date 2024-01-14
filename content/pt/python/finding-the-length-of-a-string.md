---
title:                "Python: Encontrando o comprimento de uma string"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que Encontrar o Comprimento de uma String?
Encontrar o comprimento de uma string é uma habilidade essencial na programação Python. Ao saber o comprimento de uma string, você pode determinar quantos caracteres ela contém e usá-lo em lógicas como iteração e busca em substrings. É uma das tarefas básicas que todo programador deve dominar.

## Como Fazer
Para encontrar o comprimento de uma string em Python, vamos usar a função `len()`, que é uma função interna da linguagem. Veja um exemplo simples:
```Python
nome = "Maria"
comprimento = len(nome)
print(comprimento)
```
**Saída:** 5

Neste exemplo, definimos uma string chamada `nome` e usamos a função `len()` para determinar seu comprimento. Em seguida, o valor do comprimento é impresso na tela. Simples, não é?

Vamos ver outro exemplo mais avançado. Suponha que temos um programa que pede ao usuário para digitar seu nome e, em seguida, o saúda pelo nome. Usando a função `input()`, podemos capturar o que o usuário digitou e, em seguida, encontrar o comprimento do nome antes de imprimir a saudação.
```Python
nome = input("Digite seu nome: ")
comprimento = len(nome)
print("Olá " + nome + ", seu nome tem " + str(comprimento) + " letras!")
```
**Entrada:** Maria\
**Saída:** Olá Maria, seu nome tem 5 letras!

## Deep Dive
Em Python, a função `len()` usa o protocolo de special method `__len__()` para determinar o comprimento de um objeto. Isso significa que você pode usar a função `len()` em outros tipos de dados, como listas, dicionários e até mesmo em objetos personalizados, desde que eles implementem o método `__len__()`. Portanto, a função `len()` é mais versátil do que se pode pensar à primeira vista.

## Veja Também
- [Documentação Oficial do Python sobre a Função `len()` (em inglês)](https://docs.python.org/3/library/functions.html#len)
- [Tutorial em Vídeo do Python para Iniciantes - Função `len()` (em português)](https://www.youtube.com/watch?v=1Gx3fOoelz0)
- [Livro "Introdução à Programação com Python" - Capítulo sobre Função `len()` (em português)](https://novatec.com.br/livros/introprogpython/len-de-strings-inteiros-e-listas/)