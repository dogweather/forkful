---
title:                "Maiúsculas para uma string"
html_title:           "Python: Maiúsculas para uma string"
simple_title:         "Maiúsculas para uma string"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que

Algumas vezes, é necessário capitalizar uma string em um programa Python. Isso pode ser útil em várias situações, como alterar a formatação de nomes próprios ou palavras-chave.

## Como Fazer

Usando o método `.capitalize()` é possível capitalizar uma string. Veja um exemplo abaixo:

```python
texto = "python é uma linguagem de programação"
print(texto.capitalize())
```

O output desse código será `Python é uma linguagem de programação`, com a primeira letra maiúscula.

### Outras formas de capitalizar uma string

Além do `.capitalize()`, existem outras formas de capitalizar uma string em Python, como:

- `.title()`: este método irá capitalizar cada palavra na string;
- `.upper()`: este método irá transformar a string em caixa alta;
- E também é possível usar o módulo `string.capwords()` para capitalizar cada palavra em uma string.

## Aprofundando-se

Quando usamos o método `.capitalize()`, apenas a primeira letra da string é capitalizada. Porém, é importante lembrar que esse método apenas funciona para strings com letras minúsculas. Se a string já contém letras maiúsculas, o método não terá efeito.

## Veja também

- [Documentação oficial sobre o método `.capitalize()`](https://docs.python.org/3/library/stdtypes.html#str.capitalize)
- [Tutorial sobre formatação de strings em Python](https://www.tutorialspoint.com/python/string_capitalize.htm)