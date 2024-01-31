---
title:                "Verificando se um diretório existe"
date:                  2024-01-20T14:58:16.704102-07:00
html_title:           "Fish Shell: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Verificar a existência de um diretório em Python é checar se um certo caminho corresponde a uma pasta real no sistema de arquivos. Programadores fazem isso para evitar erros ao tentar acessar, ler ou escrever arquivos em diretórios que não existem.

## Como Fazer:

Para checar se um diretório existe:

```python
import os

# Verifica se um diretório existe
diretorio = "/caminho/para/seu/diretorio"
existe = os.path.isdir(diretorio)

print(f"O diretório existe? {'Sim' if existe else 'Não'}")
```

Saída esperada pode ser "Sim" ou "Não" dependendo se o diretório existe.

Se estiver usando Python 3.5 ou superior, você também pode usar `pathlib`:

```python
from pathlib import Path

# Verifica se um diretório existe
diretorio = Path("/caminho/para/seu/diretorio")
existe = diretorio.is_dir()

print(f"O diretório existe? {'Sim' if existe else 'Não'}")
```

Novamente, a saída vai ser "Sim" ou "Não".

## Aprofundamento

Historicamente, o módulo `os` foi a forma padrão de interagir com o sistema operacional, onde `os.path.isdir()` é uma função frequente para verificações de diretório. Com a evolução da linguagem, o módulo `pathlib`, introduzido oficialmente no Python 3.4, ofereceu uma abordagem orientada a objetos para tarefas relacionadas ao sistema de arquivos, o que inclui a verificação de diretórios. `pathlib.Path.is_dir()` não é apenas mais legível; também simplifica a manipulação de caminhos em diferentes sistemas operacionais. Vale ressaltar que, apesar das diferenças de interface, ambos `os.path` e `pathlib` são capazes de realizar a tarefa de forma eficiente.

## Veja Também

Para mais detalhes sobre manipulação de arquivos e diretórios em Python, consulte a documentação oficial:

- Documentação do `os.path`: https://docs.python.org/3/library/os.path.html
- Documentação do `pathlib`: https://docs.python.org/3/library/pathlib.html
- Guia para operações comuns em arquivos/diretórios: https://docs.python.org/3/library/os.path.html#module-os.path
