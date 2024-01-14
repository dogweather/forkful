---
title:    "Python: Lendo argumentos da linha de comando"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Por que ler argumentos de linha de comando em Python?

Ler argumentos de linha de comando é uma habilidade essencial para qualquer programador Python. Ao dominar essa técnica, você poderá tornar seus scripts e programas mais dinâmicos e interativos, permitindo que os usuários forneçam inputs diretamente ao executar o código.

## Como fazer isso?

Em python, podemos ler argumentos de linha de comando usando o módulo `sys` e a função `argv`. Primeiro, importamos o módulo `sys` em nosso código:

```python
import sys
```

Em seguida, podemos usar a função `argv` para acessar os argumentos fornecidos na linha de comando. Por exemplo, se executarmos o seguinte comando no terminal:

```bash
python my_script.py arg1 arg2
```

No código, podemos acessar os argumentos "arg1" e "arg2" da seguinte maneira:

```python
import sys

# Acessando o primeiro argumento
print(sys.argv[1]) # output: "arg1"

# Acessando o segundo argumento
print(sys.argv[2]) # output: "arg2"
```

Vale ressaltar que os argumentos são armazenados como strings e podem ser convertidos para outros tipos de dados, se necessário. Além disso, o primeiro argumento sempre será o nome do arquivo do script, por isso é comum ignorá-lo e começar a acessar os argumentos a partir do índice 1.

## Profundando no assunto

Além da função `argv`, o módulo `sys` também possui outras funções úteis para lidar com argumentos de linha de comando, como `stdin` e `stdout`. Além disso, podemos usar o módulo `argparse` para criar interfaces mais sofisticadas para lidar com argumentos e opções de linha de comando.

Outra dica importante é sempre tratar possíveis erros ao ler argumentos de linha de comando, como quando o usuário não fornece argumentos suficientes ou fornece argumentos inválidos.

## Veja também

- [Documentação oficial do módulo sys](https://docs.python.org/3/library/sys.html)
- [Documentação oficial do módulo argparse](https://docs.python.org/3/library/argparse.html)
- [Tutorial em vídeo sobre argumentos de linha de comando em Python](https://www.youtube.com/watch?v=fAV7Kf5VjNM)