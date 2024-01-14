---
title:    "Python: Verificando se um diretório existe"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que checar se um diretório existe no Python?

Se você é um programador Python, provavelmente já se deparou com a necessidade de verificar se um diretório existe antes de inserir ou manipular arquivos dentro dele. Isso é importante para evitar erros e garantir que seu código esteja funcionando corretamente. Neste artigo, explicaremos como fazer isso de forma simples e eficaz.

## Como fazer

Para verificar se um diretório existe no Python, podemos usar o módulo `os` e a função `path.exists()`. Primeiro, importamos o módulo `os` em nosso código:

```Python
import os
```

Em seguida, usamos a função `path.exists()` para verificar se o diretório existe (neste exemplo, estamos verificando se o diretório "docs" existe):

```Python
if os.path.exists("docs"):
    print("O diretório 'docs' existe.")
else:
    print("O diretório 'docs' não existe.")
```

Se o diretório existir, a mensagem "O diretório 'docs' existe." será impressa. Caso contrário, a mensagem "O diretório 'docs' não existe." será exibida. É importante lembrar que a função `path.exists()` retorna `True` se o caminho existir e `False` se não existir.

Podemos até mesmo usar a função `path.isdir()` para verificar se o caminho especificado é um diretório:

```Python
if os.path.isdir("docs"):
    print("O caminho especificado é um diretório.")
else:
    print("O caminho especificado não é um diretório.")
```

## Mergulho profundo

A função `path.exists()` faz parte do módulo `os.path`, que possui outras funções úteis para trabalhar com caminhos de diretório, como verificar se é um arquivo ou diretório, listar arquivos e diretórios em um determinado caminho, entre outras funcionalidades.

Além disso, é importante ressaltar que a função `path.exists()` também pode ser usada para verificar se um arquivo existe. Nesse caso, ela retornará `True` se o arquivo existir e `False` se não existir.

## Veja também

- Documentação oficial do módulo `os.path`: https://docs.python.org/3/library/os.path.html#module-os.path
- Como criar um diretório no Python: https://www.alura.com.br/artigos/como-criar-um-diretorio-no-python
- Como listar arquivos e diretórios com Python: https://www.devmedia.com.br/listando-arquivos-e-diretorios-com-python/38050