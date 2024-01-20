---
title:                "Lendo argumentos de linha de comando"
html_title:           "Arduino: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Ler argumentos de linha de comando em Python significa captar entradas fornecidas ao executar um programa a partir do terminal. Programadores fazem isso para permitir uma execução mais flexível e dinâmica de seus programas.

## Como Fazer:

A biblioteca sys do Python tem uma lista chamada `argv` que você pode usar para captar argumentos de linha de comando. Observe o exemplo abaixo:

```Python
import sys

print("Nome do script:", sys.argv[0])

for i, arg in enumerate(sys.argv[1:], 1):
    print(f"Argumento {i}: {arg}")
```

Se você executar `python3 meu_script.py arg1 arg2 arg3`, a saída será:

```
Nome do script: meu_script.py
Argumento 1: arg1
Argumento 2: arg2
Argumento 3: arg3
```

## Mergulho Profundo

Os argumentos de linha de comando existem desde os primeiros dias da programação. Eles permitem que o usuário personalize a execução de um script sem precisar editar o código.

Em Python, além da biblioteca sys, você também pode usar a biblioteca argparse para lidar com argumentos de linha de comando de maneira mais avançada.

Detalhes sobre a implementação: `sys.argv` é uma lista em Python, onde o primeiro item, `sys.argv[0]`, é o nome do script Python atualmente em execução. Os argumentos de linha de comando seguem este, lidos como strings.

## Veja Também

- Documentação oficial Python para `sys`: https://docs.python.org/3/library/sys.html
- Documentação oficial Python para `argparse`: https://docs.python.org/3/library/argparse.html
- Guia completo para manipulação de argumentos de linha de comando em Python: https://realpython.com/command-line-interfaces-python-argparse/
- Um guia rápido para as opções de linha de comando Python: https://www.tutorialspoint.com/python/python_command_line_arguments.htm