---
title:                "Buscando e substituindo texto"
html_title:           "Python: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O que e por que?
Substituir texto é o processo de encontrar e trocar determinados trechos de texto por outros. Isso é comumente utilizado por programadores para automatizar tarefas tediosas ou corrigir erros em massa.

## Como fazer:
Para realizar uma busca e substituição de texto em Python, utilizamos o método `replace()` que aceita dois parâmetros: o texto a ser substituído e o texto de substituição.

Exemplo:
```Python
texto = "Python é uma linguagem de programação incrível!"
print(texto.replace("Python", "JavaScript"))
```

Saída:
```
JavaScript é uma linguagem de programação incrível!
```

## Mergulho profundo:
A função `replace()` foi introduzida no Python 1.6 e tem sido amplamente utilizada desde então. Uma alternativa é usar expressões regulares (módulo `re`) para realizar substituições mais complexas. Além disso, existem outras formas de realizar busca e substituição, como o uso de bibliotecas externas ou ferramentas de linha de comando.

## Veja também:
- [Documentação oficial do método `replace()` em Python](https://docs.python.org/3/library/stdtypes.html?#str.replace)
- [Referência de expressões regulares em Python](https://docs.python.org/3/howto/regex.html)
- [Biblioteca `re` em Python](https://docs.python.org/3/library/re.html)