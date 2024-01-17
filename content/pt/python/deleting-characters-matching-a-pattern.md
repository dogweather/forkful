---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Python: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O que e por que?

Excluir caracteres que correspondem a um padrão é uma tarefa comumente realizada por programadores para limpar strings ou dados. Isso é particularmente útil ao lidar com entradas de usuário que podem conter caracteres desnecessários ou incorretos.

## Como fazer:

Para excluir caracteres correspondentes a um padrão em Python, podemos utilizar a função `re.sub()` do módulo `re`. Esta função aceita três argumentos: o padrão a ser buscado, o novo valor a ser substituído e a string na qual a substituição deve ser feita. Aqui está um exemplo de como usá-la para remover todos os números de uma string:

```python
import re

texto = "abc 123 xyz"
novo_texto = re.sub(r'\d+', '', texto)
print(novo_texto)

# saída: abc xyz
```

## Profundando:

Deletar caracteres correspondentes a um padrão tornou-se mais fácil graças ao uso de expressões regulares. Essas são sequências de caracteres que definem um padrão de busca de texto. Além da função `re.sub()`, também podemos usar a função `re.findall()` para encontrar todas as correspondências de um padrão em uma string. Outra alternativa seria utilizar o método `replace()` das strings, mas ele só permite substituir uma sequência fixa de caracteres e não suporta expressões regulares.

Além disso, ao utilizar expressões regulares, podemos usar caracteres especiais como `*` e `+` para especificar se a correspondência deve ser encontrada uma ou várias vezes, respectivamente. Existem muitos recursos disponíveis no módulo `re` do Python para ajudar a criar padrões mais complexos e avançados.

## Veja também:

- Documentação oficial do módulo `re`: https://docs.python.org/3/library/re.html
- Tutorial sobre expressões regulares com Python: https://www.w3schools.com/python/python_regex.asp