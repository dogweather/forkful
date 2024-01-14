---
title:    "Python: Encontrando o comprimento de uma string"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que encontrar o tamanho de uma string é importante?

Encontrar o tamanho de uma string é uma habilidade fundamental em programação em Python. Isso pode ser útil ao criar um programa que precise manipular texto, como um verificador de ortografia ou um programa de processamento de linguagem natural. Além disso, saber como encontrar o tamanho de uma string pode ajudá-lo a criar programas mais eficientes e elegantes.

## Como fazer isso em Python
Encontrar o tamanho de uma string em Python é bastante simples. Você pode usar a função interna `len()` e passar a string como argumento. Por exemplo:

```Python
sentence = "Olá, Mundo!"
tamanho = len(sentença)
print(tamanho)

# Saída: 12
```

Você pode até mesmo usar esta função em strings vazias:
```Python
vazia = ""
tamanho = len(vazia)
print(tamanho)

# Saída: 0
```

## Mais detalhes sobre a função `len()`
A função `len()` retorna o número de caracteres em uma string, incluindo espaços em branco e caracteres especiais. Isso é importante lembrar ao usar esta função em programas que precisam contar letras ou palavras específicas em uma string.

Também é possível usar a função `len()` em outros tipos de dados, como listas e dicionários. Isso pode ser muito útil ao trabalhar com estruturas de dados mais complexas em Python.

## Veja também
- [Documentação oficial do Python sobre a função `len()`](https://docs.python.org/3/library/functions.html#len)
- [Tutorial sobre strings em Python](https://realpython.com/python-strings/)
- [Exemplos de uso da função `len()` em diferentes situações](https://www.programiz.com/python-programming/methods/built-in/len)