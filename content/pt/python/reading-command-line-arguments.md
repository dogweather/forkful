---
title:                "Lendo argumentos da linha de comando"
html_title:           "Python: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O que e por que?
Ler argumentos da linha de comando é um processo em que um programa Python pode receber informações do usuário por meio do terminal. Os programadores muitas vezes fazem isso para possibilitar a personalização dos seus programas e interação com o usuário.

## Como fazer:
Utilize o módulo `sys` para importar a função `argv` e receber os argumentos passados na linha de comando. Em seguida, basta acessar esses argumentos por meio de índices, começando pelo primeiro após o nome do arquivo. Veja um exemplo abaixo:
```python
import sys

# Executando o programa com o comando: python programa.py arg1 arg2 arg3
argumentos = sys.argv
print(argumentos[0]) # nome do arquivo
print(argumentos[1]) # arg1
print(argumentos[2]) # arg2
print(argumentos[3]) # arg3
```
Saída: 
```
programa.py
arg1
arg2
arg3
```

## Aprofundamento:
Ler argumentos da linha de comando é uma técnica antiga, sendo comum em linguagens de programação como C e Perl. No entanto, em Python, existem outras formas de se obter entrada do usuário, como a função `input()`. Além disso, é importante sempre validar e tratar os argumentos recebidos, para evitar erros e falhas no programa.

## Veja também:
- Documentação oficial do módulo `sys`: https://docs.python.org/3/library/sys.html
- Tutorial sobre input e argumentos na linha de comando: https://www.geeksforgeeks.org/command-line-arguments-in-python/