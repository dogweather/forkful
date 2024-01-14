---
title:    "Python: Verificando se um diretório existe"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Por que verificamos se um diretório existe?

Frequentemente, ao escrever um programa em Python, pode ser necessário verificar se um determinado diretório existe antes de tentar trabalhar com ele. Isso pode ser útil para garantir que o programa não tente acessar um diretório inexistente e cause erros. Também pode ser útil para garantir que um diretório exista antes de criar um novo arquivo dentro dele.

## Como fazer

Para verificar se um diretório existe em Python, podemos usar a função `path.exists()` do módulo `os`. Vamos ver um exemplo de código:

```Python
import os

diretorio = "documentos"

if os.path.exists(diretorio):
  print("O diretório existe!")
else:
  print("O diretório não existe.")
```

Este código primeiro importa o módulo `os`, que contém funções relacionadas a sistemas operacionais. Em seguida, definimos a variável `diretorio` com o nome do diretório que queremos verificar. Então, usamos a função `path.exists()` para verificar se o diretório existe. Se existir, o programa imprimirá "O diretório existe!". Caso contrário, imprimirá "O diretório não existe.".

## Deep Dive

A função `path.exists()` verifica se um determinado caminho existe no sistema de arquivos atual. Portanto, pode ser usado para verificar não apenas a existência de diretórios, mas também de arquivos. É importante notar que essa função só retornará `True` se o caminho especificado for um diretório ou arquivo real, e não apenas um caminho válido. Isso significa que, mesmo que o caminho seja válido, se não houver um diretório ou arquivo correspondente, a função sempre retornará `False`.

## Veja também

- Documentação oficial do módulo `os`: https://docs.python.org/3/library/os.html
- Tutorial sobre como trabalhar com diretórios em Python: https://www.geeksforgeeks.org/python-os-path-module/
- Exemplo de código para criar um novo diretório em Python: https://www.programiz.com/python-programming/directory