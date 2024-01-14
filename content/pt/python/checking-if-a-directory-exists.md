---
title:                "Python: Verificando se um diretório existe."
simple_title:         "Verificando se um diretório existe."
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Verificar se um diretório existe é uma tarefa importante em programação, pois nos permite confirmar se um caminho especificado existe ou não, antes de tentarmos realizar operações como leitura ou gravação de arquivos nesse caminho. Isso evita erros e falhas inesperadas no código.

## Como fazer:

Para verificar a existência de um diretório em Python, podemos utilizar a função `path.exists()` do módulo `os` ou `pathlib.Path()`. Veja um exemplo abaixo:

```Python
import os
import pathlib

# Verificando com o módulo `os`
if os.path.exists("meu_diretorio"):
     print("O diretório existe!")
else:
     print("O diretório não existe!")

# Verificando com a classe `Path` do módulo `pathlib`
if pathlib.Path("meu_diretorio").exists():
     print("O diretório existe!")
else:
     print("O diretório não existe!")
```

Ambas as opções nos retornam um valor booleano (True ou False) indicando se o diretório existe ou não.

## Profundando:

Além de verificar a existência de um diretório, podemos também obter mais informações sobre ele, como por exemplo, se é um arquivo ou um diretório, utilizando as funções `os.path.isfile()` e `os.path.isdir()`. Podemos também usar a função `os.listdir()` para obter uma lista dos arquivos e diretórios dentro do diretório especificado. Veja um exemplo abaixo:

```Python
import os

# Verificando se é um arquivo ou um diretório
if os.path.isfile("meu_arquivo.txt"):
    print("É um arquivo!")
elif os.path.isdir("meu_diretorio"):
    print("É um diretório!")

# Obtendo a lista de conteúdo dentro do diretório
conteudo = os.listdir("meu_diretorio")
print("Conteúdo do diretório: ", conteudo)
```

## Veja também:
- Documentação oficial do Python sobre o módulo `os`: https://docs.python.org/3/library/os.path.html
- Documentação oficial do Python sobre o módulo `pathlib`: https://docs.python.org/3/library/pathlib.html
- Tutorial sobre o uso de paths em Python: https://realpython.com/python-pathlib/
- Videoaula explicando como verificar a existência de um diretório em Python: https://www.youtube.com/watch?v=g-zoDLZLNo0