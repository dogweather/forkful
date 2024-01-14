---
title:                "Python: Lendo argumentos da linha de comando"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando?

Ler argumentos da linha de comando é uma habilidade crucial para quem está aprendendo a programar em Python. Isso permite que você interaja com um programa através de parâmetros passados na linha de comando, simplificando o processo de execução e facilitando o uso em diferentes contextos.

## Como ler argumentos da linha de comando

Para ler argumentos da linha de comando em Python, utilizamos a biblioteca `sys`, que fornece a função `argv`. Esta função retorna uma lista contendo todos os argumentos passados na linha de comando, incluindo o próprio nome do arquivo Python.

Vamos ver um exemplo simples de como utilizar essa função:

```Python
import sys
# Supondo que o arquivo python seja chamado "exemplo.py"
print(sys.argv[0]) # Saída: "exemplo.py"
print(sys.argv[1]) # Primeiro argumento passado: "Olá"
print(sys.argv[2]) # Segundo argumento passado: "mundo!"
```

Podemos passar quantos argumentos desejarmos na linha de comando, eles serão armazenados na lista `sys.argv`.

## Mais informações sobre a leitura de argumentos da linha de comando

Ao utilizar a função `argv`, é importante lembrar que todos os argumentos são considerados como strings. Isso significa que é necessário realizar conversões de tipo, caso seja necessário utilizar os valores passados em operações matemáticas, por exemplo.

Além disso, é possível utilizar a biblioteca `argparse`, que fornece uma forma mais sofisticada de lidar com argumentos da linha de comando, permitindo até mesmo a criação de opções e argumentos com valores predefinidos.

Não deixe de consultar a documentação oficial do Python para mais informações e exemplos sobre a leitura de argumentos da linha de comando.

## Veja também

- [Documentação oficial do Python sobre a função `argv`](https://docs.python.org/3/library/sys.html#sys.argv)
- [Tutorial sobre a biblioteca `argparse`](https://docs.python.org/3/howto/argparse.html)