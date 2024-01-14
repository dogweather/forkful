---
title:                "Python: Excluindo caracteres que correspondem a um padrão."
simple_title:         "Excluindo caracteres que correspondem a um padrão."
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Porquê

Às vezes, ao trabalhar com dados, podemos nos deparar com a necessidade de remover caracteres que correspondem a um determinado padrão. Isso pode ser útil quando estamos lidando com strings que contêm informações desnecessárias ou indesejadas.

## Como Fazer

Para deletar os caracteres correspondentes a um padrão em uma string, podemos usar a função `re.sub()` do módulo `re` do Python. Esta função substitui todas as ocorrências do padrão especificado por uma string vazia, efetivamente excluindo-as da string original.

Aqui está um exemplo de como usar `re.sub()` para remover todas as letras maiúsculas de uma string:

```python
import re

string = "Olá amigo! Bem-Vindo ao Meu Blog."
nova_string = re.sub("[A-Z]", "", string)

print(nova_string)
```

**Output:**
```
lá amigo! em-vindo ao eu log.
```

No exemplo acima, usamos uma expressão regular entre colchetes para indicar que queremos substituir todas as letras maiúsculas na string pela string vazia. É importante lembrar que a função `re.sub()` é sensível a maiúsculas e minúsculas, então `[A-Z]` não vai corresponder às letras minúsculas.

Além disso, podemos usar quantificadores para indicar quantas ocorrências do padrão queremos substituir. Por exemplo, se quisermos remover apenas as duas primeiras letras maiúsculas da string, podemos usar `[A-Z]{2}` no lugar de `[A-Z]`.

## Deep Dive

O módulo `re` do Python nos fornece uma série de opções para criar expressões regulares mais complexas e utilizar funções como `re.sub()` de forma mais eficiente. Alguns exemplos incluem usar meta caracteres, como `*`, `+` e `?`, para fazer correspondências mais flexíveis e usar grupos de captura para extrair informações específicas de uma string.

Para uma explicação mais detalhada sobre expressões regulares, recomenda-se a leitura da documentação oficial do Python ou outros recursos online, como o tutorial do W3Schools.

## Veja Também

- [Documentação oficial do módulo re do Python](https://docs.python.org/pt-br/3/library/re.html)
- [Tutorial de expressões regulares do W3Schools](https://www.w3schools.com/python/python_regex.asp)
- [Guia de expressões regulares do Real Python (em inglês)](https://realpython.com/regex-python/)