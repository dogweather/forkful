---
date: 2024-01-26 04:17:19.761019-07:00
description: "Um REPL, ou Loop de Leitura-Avalia\xE7\xE3o-Impress\xE3o, \xE9 um ambiente\
  \ de programa\xE7\xE3o que recebe entradas \xFAnicas do usu\xE1rio, executa-as e\
  \ retorna o resultado ao\u2026"
lastmod: '2024-02-25T18:49:43.817724-07:00'
model: gpt-4-0125-preview
summary: "Um REPL, ou Loop de Leitura-Avalia\xE7\xE3o-Impress\xE3o, \xE9 um ambiente\
  \ de programa\xE7\xE3o que recebe entradas \xFAnicas do usu\xE1rio, executa-as e\
  \ retorna o resultado ao\u2026"
title: Usando um shell interativo (REPL)
---

{{< edit_this_page >}}

## O Que & Por Que?
Um REPL, ou Loop de Leitura-Avaliação-Impressão, é um ambiente de programação que recebe entradas únicas do usuário, executa-as e retorna o resultado ao usuário. Programadores usam isso para testes rápidos, aprendizado, depuração ou fazer cálculos de imediato.

## Como Fazer:
Entre diretamente no REPL do Python digitando `python` em sua linha de comando. Uma vez lá, teste operações simples ou código com várias linhas:

```Python
>>> 1 + 1
2
>>> for i in range(3):
...     print(i)
... 
0
1
2
```

Experimente com funções e feedback imediato:

```Python
>>> def greet(name):
...     return "Olá, " + name + "!"
... 
>>> greet("Alice")
'Olá, Alice!'
```

Brinque com bibliotecas e explore suas características em tempo real:

```Python
>>> import math
>>> math.sqrt(16)
4.0
```

Saia com um rápido `exit()` ou `Ctrl+D` (às vezes `Ctrl+Z` no Windows).

## Aprofundamento
O conceito de um REPL não é único para o Python; é tão antigo quanto Lisp. Muitas linguagens oferecem esse ambiente imediato e interativo para uma abordagem prática do código. Alternativas ao shell nativo do Python incluem IPython e Jupyter Notebook, que fornecem maior interatividade, mais recursos e melhor integração com outras ferramentas. O REPL padrão do Python é simples, mas incorpora todo o poder do Python, lidando com objetos complexos e programas multithreaded, embora lhe faltem recursos como auto-completação e realce de sintaxe presentes em ferramentas mais avançadas.

## Veja Também
- [Documentação oficial do Python sobre o interpretador](https://docs.python.org/3/tutorial/interpreter.html)
- [IPython: Um shell Python avançado](https://ipython.org/)
- [Projeto Jupyter](https://jupyter.org/)
