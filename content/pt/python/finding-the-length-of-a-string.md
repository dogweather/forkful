---
title:                "Encontrando o comprimento de uma string."
html_title:           "Python: Encontrando o comprimento de uma string."
simple_title:         "Encontrando o comprimento de uma string."
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O que e por que?

Encontrar o comprimento de uma string é uma tarefa comum em programação, onde o objetivo é determinar o número de caracteres de uma determinada string. Isso é útil para manipulação e processamento de texto, além de ser uma habilidade fundamental para a resolução de problemas em diversos projetos de programação.

## Como fazer:

```python
# Exemplo 1 - Usando a função len()
texto = "Olá, mundo!"
print(len(texto))

# Saída: 12

# Exemplo 2 - Usando um loop for
texto = "Python"
contador = 0
for letra in texto:
    contador += 1
print(contador)

# Saída: 6
```

## Mergulho Profundo:

Encontrar o comprimento de uma string é uma tarefa que remonta aos primeiros dias da programação de computadores. Antigamente, era uma tarefa mais complicada e exigia algoritmos complexos. No entanto, com o avanço da tecnologia, essa tarefa se tornou muito mais fácil e simples de ser realizada. Além disso, existem outras formas de encontrar a extensão de uma string, como utilizando a função len() na maioria das linguagens de programação.

## Veja também:

- Documentação oficial do Python sobre strings: https://docs.python.org/3/library/stdtypes.html#str
- Tutorial sobre manipulação de strings em Python: https://www.w3schools.com/python/python_strings.asp
- Discussão sobre a diferença entre a função len() e a propriedade len em Python: https://stackoverflow.com/questions/26525498/difference-between-len-function-and-len-attribute-in-python