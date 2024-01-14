---
title:                "Python: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Por que ler argumentos da linha de comando é importante

Ler argumentos da linha de comando é uma habilidade essencial para qualquer programador Python. Isso permite que o seu programa interaja com o usuário e receba informações importantes diretamente da linha de comando, tornando-o mais flexível e versátil.

# Como fazer

Para ler argumentos da linha de comando em Python, é necessário utilizar a biblioteca `sys`. Primeiramente, você deve importar essa biblioteca no seu código. Em seguida, é possível acessar os argumentos utilizando a função `sys.argv`.

Vamos ver um exemplo de como ler argumentos da linha de comando em Python:

```Python
import sys

# Verificando se foram passados pelo menos dois argumentos
if len(sys.argv) >= 2:
    # Acessando o primeiro argumento
    print("O primeiro argumento é:", sys.argv[1])

    # Acessando o segundo argumento
    print("O segundo argumento é:", sys.argv[2])
else:
    # Caso não sejam passados argumentos suficientes
    print("Pelo menos dois argumentos precisam ser informados!")
```

Se rodarmos esse código na linha de comando, passando dois argumentos separados por um espaço, o resultado será:

```
$ python3 argumentos.py argumento1 argumento2
O primeiro argumento é: argumento1
O segundo argumento é: argumento2
```

Agora, se não passarmos argumentos suficientes, o resultado será:

```
$ python3 argumentos.py argumento1
Pelo menos dois argumentos precisam ser informados!
```

# Mergulho profundo

A função `sys.argv` retorna uma lista contendo os argumentos passados na linha de comando. Isso significa que, se desejar, é possível acessá-los diretamente utilizando seus índices. Por exemplo, `sys.argv[0]` retornará o nome do arquivo do seu programa.

Além disso, é possível utilizar a biblioteca `argparse` para criar uma interface mais amigável para receber argumentos da linha de comando. Essa biblioteca permite definir argumentos obrigatórios, opções, argumentos posicionais e mais, facilitando o processo de ler e validar argumentos.

# Veja também

- [Documentação da biblioteca `sys` em português](https://docs.python.org/pt-br/3/library/sys.html)
- [Documentação da biblioteca `argparse` em português](https://docs.python.org/pt-br/3/howto/argparse.html)
- [Tutorial sobre argparse em português](https://pythonhelp.wordpress.com/2012/08/13/python-argumentos-da-linha-de-comando-utilizando-argparse/)