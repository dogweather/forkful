---
title:                "Convertendo uma string para minúsculas"
html_title:           "Fish Shell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Converting Strings para Minúsculas em Python: Um Guia Prático

## O Que & Por Quê?

Converter uma string para minúsculas em Python significa transformar todas as letras maiúsculas em letras minúsculas. Programadores fazem isso para minimizar erros de comparação de strings e normalizar os dados de entrada.

## Como Fazer:

Em Python, o método `lower()` é usado para converter todas as letras maiúsculas em uma string para minúsculas.

Aqui está um exemplo:

```Python
s = "OLÁ MUNDO!"
print(s.lower())
```

A saída será:

```Python
"olá mundo!"
```

## Mergulho Profundo

A função `lower()` tem sido um recurso do Python desde as versões iniciais da linguagem devido à sua utilidade em manipulação de texto.

Existem alternativas, como usar list comprehensions para iterar sobre cada caractere. Mas esses métodos são geralmente mais complexos e menos eficientes.

Por baixo dos panos, a função `lower()` faz uso da tabela UNICODE para localizar a contraparte em minúscula de cada caractere maiúsculo.

## Veja Também

Para mais informações e exemplos do método `lower()`, verifique os seguintes recursos:

- Documentação Oficial do Python: [str.lower()](https://docs.python.org/3/library/stdtypes.html#str.lower)
- W3Schools Python Strings: Lowercase Methods: [Python Lower()](https://www.w3schools.com/python/ref_string_lower.asp)
- GeeksforGeeks: [Python String | lower()](https://www.geeksforgeeks.org/python-string-lower/)