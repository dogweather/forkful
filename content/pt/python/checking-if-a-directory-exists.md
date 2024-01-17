---
title:                "Verificando se um diretório existe"
html_title:           "Python: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

O que & Por quê?
Verificar se um diretório existe é uma forma de garantir que um diretório específico está presente no sistema de arquivos. Os programadores fazem isso para evitar erros e bugs em seus códigos, garantindo que a execução do programa seja interrompida caso o diretório não exista.

Como fazer:
Verificar se um diretório existe é bastante simples em Python, basta usar a função `os.path.exists()` e passar o caminho do diretório como argumento. A função retornará um valor booleano: `True` se o diretório existir e `False` caso contrário.

```
Python
import os

# Verificar se o diretório "imagens" existe
if os.path.exists("imagens"):
    print("O diretório 'imagens' existe!")
else:
    print("O diretório 'imagens' não existe!")
```

Output:
```
O diretório 'imagens' existe!
```

Deep Dive:
Historicamente, verificar se um diretório existe era feito usando a função `os.path.isdir()`, que verificava se um caminho era um diretório. No entanto, essa função poderia retornar `True` para links simbólicos, o que não era o comportamento desejado em alguns casos. Por isso, a função `os.path.exists()` foi criada para verificar se um caminho está presente no sistema de arquivos, sem se importar se é um arquivo ou diretório.

Alternativas para verificar se um diretório existe incluem o uso do módulo `pathlib` e a função `os.access()`, que verifica se um caminho tem permissões específicas de acesso.

Veja também:
- Documentação oficial do Python sobre a função `os.path.exists()`: https://docs.python.org/3/library/os.path.html#os.path.exists
- Artigo sobre diferentes maneiras de testar a existência de um diretório em Python: https://www.geeksforgeeks.org/python-check-if-a-file-or-directory-exists-2/