---
title:                "Escrevendo no erro padrão"
html_title:           "Arduino: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Escrever no erro padrão (stderr) é como mandar mensagens de erro ou diagnóstico sem misturar com a saída normal do seu programa. Programadores fazem isso para separar as streams, facilitando o rastreamento de bugs e o processamento de saídas válidas.

## How to:
```python
import sys

print("Isto é uma saída normal.")
sys.stderr.write("Isto é uma mensagem de erro.\n")

try:
    # Simulando uma operação que causa um erro.
    1 / 0
except ZeroDivisionError as e:
    print(f"Erro: {e}", file=sys.stderr)
```

Saída esperada seria algo parecido com:
```
Isto é uma saída normal.
Isto é uma mensagem de erro.
Erro: division by zero
```

## Deep Dive
A separação entre stderr e stdout remonta aos primeiros dias dos sistemas UNIX, permitindo que operações de pipe e redirecionamento tratassem saídas de erro e normais de forma distinta. Alternativas modernas incluem o uso de bibliotecas de log, que oferecem maior controle e opções de filtragem. Quando se escreve em stderr em Python, o sistema não faz buffer da saída, o que significa que as mensagens de erro são exibidas imediatamente.

## See Also
- Documentação da biblioteca `sys` do Python: https://docs.python.org/3/library/sys.html
- Guia Python sobre logging: https://docs.python.org/3/howto/logging.html
- Stack Overflow sobre stderr e stdout: https://stackoverflow.com/questions/3385201/confused-about-stdin-stdout-and-stderr