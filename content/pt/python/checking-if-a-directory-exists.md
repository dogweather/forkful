---
title:                "Verificando se um diretório existe"
html_title:           "Javascript: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Verificar se um diretório existe é um processo em que verificamos se um determinado local em nosso sistema de arquivos existe antes de usar. Fazemos isso para evitar erros durante a execução de nossos programas, como tentar acessar um local que não existe.

## Como Fazer:

Usando o módulo os na biblioteca padrão do Python, podemos verificar facilmente se um diretório existe.

```Python
import os

diretorio = "/caminho/para/o/diretorio"

if os.path.isdir(diretorio):
    print("O diretório existe.")
else:
    print("O diretório não existe.")
```

Nesse código, o método `os.path.isdir()` retorna `True` se o diretório existir e `False` caso contrário.

## Mergulho Profundo

Verificar se um diretório existe é um conceito que vem desde o início da programação, com diversas implementações em várias linguagens de programação.

Uma alternativa ao método `os.path.isdir()` é usar a função `os.path.exists()`, que verifica se um caminho existe. Esta função retorna `True` para arquivos e diretórios, então é um pouco mais ampla.

```Python
import os

path = "/caminho/para/o/diretorio"

if os.path.exists(path):
    print("O caminho existe.")
else:
    print("O caminho não existe.")
```

Em termos de implementação, o Python utiliza internamente chamadas de sistema para verificar a existência do diretório. Essas chamadas de sistema são específicas para cada sistema operacional e por isso o Python consegue ser uma linguagem de programação portátil.

## Veja Também

- Documentação oficial do Python sobre o módulo os [aqui](https://docs.python.org/pt-br/3/library/os.path.html#os.path.isdir)
- Diferença de uso entre os.path.exists, os.path.isfile, os.path.isdir [aqui](https://stackoverflow.com/questions/82831/how-do-i-check-whether-a-file-exists-without-exceptions)
- Explicação do Sistema de Arquivos do Python [aqui](https://docs.python.org/pt-br/3/tutorial/stdlib2.html#the-file-system)