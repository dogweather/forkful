---
title:                "Python: Escrevendo no erro padrão"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Por que escrever no "standard error"

Escrever no "standard error" é uma prática comum na programação em Python, principalmente para depurar e lidar com erros em nossos códigos. Quando um programa é executado, é importante entender os diferentes tipos de saída que podem ser gerados, incluindo a saída padrão (standard output) e a saída de erro (standard error). Neste artigo, vamos explorar o uso e a importância da escrita no "standard error" em nossos programas.

## Como fazer

Para escrever no "standard error" em Python, podemos usar a função `sys.stderr.write()`, que escreve uma mensagem de erro diretamente no "standard error" em vez de exibi-la na tela. Vamos ver um exemplo simples:

```Python
import sys

numero = input("Digite um número inteiro: ")

try:
    resultado = int(numero)
    print("O quadrado de", numero, "é", resultado ** 2)
except ValueError:
    sys.stderr.write("ERRO: Você não digitou um número inteiro!")
```

Neste exemplo, se o usuário digitar um valor que não possa ser convertido para inteiro, a mensagem de erro será exibida no "standard error" em vez de aparecer na tela junto com o resultado da operação. Isso pode ser especialmente útil em programas maiores, onde é importante distinguir entre a saída normal e possíveis mensagens de erro.

## Mergulho Profundo

Uma das principais razões pelas quais escrevemos para o "standard error" é para ajudar no processo de depuração de nossos códigos. Quando encontramos um erro em nosso programa, é importante entender o que causou o problema e onde ele ocorreu no código. Ao escrever mensagens de erro no "standard error", podemos identificar facilmente onde o erro ocorreu e como ele pode ser corrigido.

Além disso, ao escrever no "standard error", podemos controlar melhor a saída de nossos programas. Enquanto a saída padrão é geralmente usada para mostrar os resultados do programa, a saída de erro pode ser usada para exibir informações adicionais ou mensagens de aviso que não são diretamente relacionadas ao resultado.

## Veja também

- [Documentação oficial do Python sobre sys.stderr](https://docs.python.org/3/library/sys.html#sys.stderr)
- [Tutorial sobre como lidar com erros em Python](https://realpython.com/python-exceptions/)
- [Artigo sobre como depurar programas em Python](https://blog.finxter.com/how-to-debug-python-code/)