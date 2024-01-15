---
title:                "Verificando se um diretório existe."
html_title:           "Python: Verificando se um diretório existe."
simple_title:         "Verificando se um diretório existe."
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que
Algumas vezes, ao trabalhar com arquivos em um programa Python, pode ser necessário verificar se um diretório existe antes de realizar alguma operação. Isso pode ser útil para evitar erros e garantir que o programa funcione corretamente.

## Como fazer
Para verificar se um diretório existe em Python, podemos utilizar o módulo `os` e a função `path.exists()`. Veja um exemplo de código abaixo:

```Python
import os

# Verifica se o diretório "meus_arquivos" existe
if os.path.exists("meus_arquivos"):
    print("O diretório existe!")
else:
    print("O diretório não existe!")
```

O código acima irá verificar se o diretório "meus_arquivos" existe e, caso exista, irá imprimir a mensagem "O diretório existe!". Caso contrário, irá imprimir a mensagem "O diretório não existe!".

## Aprofundando
Além da função `path.exists()`, existem outras opções para verificar a existência de um diretório em Python. Por exemplo, podemos utilizar a função `path.isdir()` para verificar se o caminho passado como parâmetro é realmente um diretório.

```Python
import os

# Verifica se o caminho é um diretório
if os.path.isdir("/home/usuario/meus_arquivos"):
    print("O caminho é um diretório!")
else:
    print("O caminho não é um diretório!")
```

Também é possível utilizar a função `path.isabs()` para verificar se o caminho passado como parâmetro é absoluto (começa com `/`) ou relativo.

```Python
import os

# Verifica se o caminho é absoluto
if os.path.isabs("meus_arquivos"):
    print("O caminho é absoluto!")
else:
    print("O caminho é relativo!")
```

Outra opção é utilizar o módulo `glob` e a função `glob.glob()` para fazer uma busca por diretórios que correspondam a um determinado padrão.

```Python
import glob

# Busca por diretórios que iniciam com "meus"
meus_dirs = glob.glob("meus*")

print(meus_dirs)
# Saída: ['meus_arquivos', 'meus_documentos', 'meus_videos']
```

Ficar atento a essas opções pode ser útil dependendo da situação em que você precisa verificar a existência de um diretório.

## Veja também
- [Documentação oficial do módulo `os`](https://docs.python.org/3/library/os.html)
- [Documentação oficial do módulo `glob`](https://docs.python.org/3/library/glob.html)