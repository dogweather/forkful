---
title:                "Concatenando strings"
html_title:           "Python: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## O que é e por que fazer isso?
Concatenar strings significa unir várias strings em uma só. Os programadores geralmente fazem isso para formar uma frase ou mensagem completa a partir de strings separadas.

## Como fazer:
Para concatenar strings em Python, é possível utilizar o operador "+" ou o método ".join()". Veja alguns exemplos:

```python
# Com o operador "+":
string1 = "Olá "
string2 = "mundo!"
frase = string1 + string2
print(frase) # Output: "Olá mundo!"

# Com o método ".join()":
lista = ["Hello", "world!"]
frase = " ".join(lista)
print(frase) # Output: "Hello world!"

# Também é possível concatenar um número variável de strings:
nome = "Ana"
idade = 20
mensagem = "Meu nome é " + nome + " e tenho " + str(idade) + " anos."
print(mensagem) # Output: "Meu nome é Ana e tenho 20 anos."
```

## Mergulho profundo:
A concatenação de strings é uma técnica muito utilizada em programação e existem várias formas de fazê-la. Além das opções apresentadas acima, é possível utilizar o método "format()" ou as f-strings na versão mais recente do Python (3.6+). Cada uma dessas abordagens possui suas vantagens e pode ser mais adequada dependendo do contexto.

## Veja também:
- Documentação oficial do Python sobre concatenação de strings: https://docs.python.org/3/library/string.html#formatstrings
- Artigo sobre as f-strings: https://realpython.com/python-f-strings/
- Vídeo explicando diferentes formas de concatenar strings em Python: https://www.youtube.com/watch?v=aPjNU_ipK7Q