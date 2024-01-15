---
title:                "Convertendo uma string para letras minúsculas"
html_title:           "Python: Convertendo uma string para letras minúsculas"
simple_title:         "Convertendo uma string para letras minúsculas"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que

Convertendo strings em letras minúsculas pode ser útil em várias situações, como por exemplo quando se está trabalhando com dados que são sensíveis à capitalização.

## Como fazer

Para converter uma string em letras minúsculas em Python, você pode usar o método `lower()` em uma string. Veja um exemplo abaixo:

```Python
string = "CONVERTER PARA MINÚSCULAS"
string_lower = string.lower()
print(string_lower)
```
O resultado será: `converter para minúsculas`

Você também pode usar o método `casefold()` para realizar a mesma conversão, porém com a diferença de que ele também lida com caracteres Unicode em diferentes idiomas. Veja o exemplo abaixo:

```Python
string = "ConVerTer parA MiNúsculaS ツ"
string_lower = string.casefold()
print(string_lower)
```
O resultado será: `converter para minúsculas ツ`

## Mergulho Profundo

Ao converter uma string para letras minúsculas, é importante entender como a linguagem Python lida com a capitalização de letras. Em Python, strings são consideradas como objetos imutáveis, portanto o método `lower()` cria uma nova string contendo as letras minúsculas da original, ao invés de alterar a string original em si. Isso significa que, se você quiser armazenar a string em letras minúsculas, você precisa atribuí-la a uma nova variável.

Você também pode usar a função `str.lower()` para realizar a mesma tarefa, porém ela só funciona para strings. Já o método `lower()` pode ser usado em outros tipos de objetos, como listas ou dicionários, onde ele vai converter todas as strings contidas nesses objetos em letras minúsculas.

## Veja também

- Documentação oficial do Python sobre strings, que inclui mais informações sobre os métodos `lower()` e `casefold()`: https://docs.python.org/3/library/string.html
- Um artigo detalhado sobre as diferenças entre o método `lower()` e `casefold()`: https://waymoot.org/home/python_string/