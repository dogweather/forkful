---
title:                "Concatenando strings"
html_title:           "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/concatenating-strings.md"
---

{{< edit_this_page >}}

# Concatenação de Strings em Python: Como funciona e Por que É Importante

## O Que É e Por Quê?

Concatenação de strings se refere a juntar duas ou mais strings para formar uma nova. É útil para unir mensagens de texto, formatar saídas e processar dados de texto.

## Como concatenar: 

No Python, aqui estão algumas formas comuns de concatenar strings:

```Python
# usando o operador '+'
a = "Hello"
b = "World"
print(a + " " + b)  # output: 'Hello World'

# usando o método 'join()'
strings = ['Hello', 'World']
print(" ".join(strings))  # output: 'Hello World'

# usando f-string (Python 3.6+)
name = 'João'
print(f'Hello {name}')  # output: 'Hello João' 
```

## Mergulhando a fundo

Combinei operações de string remontam aos primeiros dias da programação, antes mesmo do Python existir. No Python, concatenação de string é otimizada para ser rápida e eficiente.

Existe uma grande variedade de formas para concatenar strings em Python. Além das mencionadas, ainda temos interpolação de strings, formatação de strings com `%` e a função `format()` do Python.

No entanto, nestas várias maneiras, a eficiência pode variar. Para um grande número de strings, o método `join()` é geralmente mais rápido do que o uso do operador `+`, principalmente porque o operador `+` cria um novo objeto de string cada vez que é usado, enquanto `join()` só cria uma nova string no final.

## Veja também 

- [Documentação Oficial Python sobre Concatenação de Strings](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)
- [Artigo sobre otimização de concatenação de strings](https://waymoot.org/home/python_string/)
- [Tutorial sobre Formatação de Strings](https://realpython.com/python-f-strings/)