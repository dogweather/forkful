---
title:    "Python: Utilizando expressões regulares"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares?

Expressões regulares são uma das ferramentas mais poderosas do Python para processamento de texto. Com elas, você pode encontrar padrões em strings, fazer substituições e até mesmo validar entradas de usuários. Em resumo, é uma forma extremamente eficiente de lidar com texto em suas aplicações.

## Como usar expressões regulares no Python

Usar expressões regulares no Python é fácil e simples. Primeiro, é necessário importar o módulo `re`:

```Python
import re
```

Em seguida, podemos utilizar a função `search()` para encontrar um padrão específico em uma string:

```Python
# Encontrar a palavra "expressão" em uma string
texto = "Estudando expressões regulares no Python"
padrao = re.search("expressão", texto)
print(padrao.group()) # Saída: expressão
```

Outra forma comum de utilizar expressões regulares é através da função `sub()`, que permite substituir um padrão por outro valor:

```Python
# Substituir todos os números em uma string por asteriscos
texto = "12345 abc 67890"
novo_texto = re.sub("[0-9]", "*", texto)
print(novo_texto) # Saída: ***** abc *****
```

## Aprofundando nas expressões regulares

As expressões regulares do Python são baseadas em uma linguagem chamada Regex, que possui uma sintaxe própria para encontrar e manipular padrões em uma string. Algumas das principais sequências especiais do Regex são:

- `\d`: encontra qualquer dígito
- `\w`: encontra qualquer caractere alfanumérico
- `[a-z]`: encontra qualquer letra minúscula
- `+`: encontra uma ou mais ocorrências
- `*`: encontra zero ou mais ocorrências
- `?`: encontra zero ou uma ocorrência

Além disso, é possível utilizar parênteses para agrupar padrões e utilizar métodos como `split()` e `findall()` para manipular as strings encontradas.

## Veja também

- [Documentação oficial do módulo `re`](https://docs.python.org/3/library/re.html)
- [Tutorial de expressões regulares no Python](https://www.treinaweb.com.br/blog/o-que-sao-expressoes-regulares-e-como-usa-las-em-python/)