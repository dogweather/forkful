---
title:                "Python: Concatenando strings"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que concatenar strings?

A concatenação de strings é uma das tarefas mais básicas da programação e é frequentemente usada para combinar informações e criar mensagens personalizadas. É especialmente útil em projetos de desenvolvimento web, onde precisamos criar dinamicamente conteúdos para exibir aos usuários.

## Como fazer

A concatenação de strings é uma tarefa simples em Python. Veja um exemplo básico de como concatenar duas strings:

```Python
nome = "Maria"
sobrenome = "Silva"
nome_completo = nome + " " + sobrenome
print(nome_completo) # Maria Silva
```

Além disso, é possível utilizar o método `.format()` para inserir valores em uma string já existente. Veja um exemplo:

```Python
nome = "João"
idade = 25
mensagem = "Olá {}, você tem {} anos.".format(nome, idade)
print(mensagem) # Olá João, você tem 25 anos.
```

Também é possível utilizar a f-string, que é uma forma mais moderna e legível de fazer a concatenação de strings em Python 3. Veja um exemplo:

```Python
nome = "Ana"
idade = 30
mensagem = f"Olá {nome}, você tem {idade} anos."
print(mensagem) # Olá Ana, você tem 30 anos.
```

## Aprofundando-se

Existem algumas coisas a serem consideradas ao concatenar strings em Python. Primeiro, é importante saber que o operador `+` é usado para a concatenação, mas também pode ser usado para adição matemática. Portanto, é necessário garantir que os valores a serem concatenados são do tipo string.

Além disso, métodos como `.format()` e f-strings permitem a inserção de valores formatados em uma string, o que pode ser muito útil e eficiente em certas situações.

Por fim, ao trabalhar com grandes quantidades de strings para concatenação, é recomendável utilizar o método `join()` para aumentar a eficiência do código.

## Veja também

- [Documentação oficial da linguagem Python](https://www.python.org/doc/)
- [Tutorial de concatenação de strings em Python](https://www.geeksforgeeks.org/python-programming-language/)
- [Diferença entre f-strings e `.format()`](https://realpython.com/python-f-strings/)