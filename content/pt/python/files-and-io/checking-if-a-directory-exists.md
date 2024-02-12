---
title:                "Verificando se um diretório existe"
date:                  2024-02-03T19:08:13.860175-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verificando se um diretório existe"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?
Verificar se um diretório existe em Python consiste em confirmar a presença de uma pasta no sistema de arquivos antes de realizar operações como ler ou escrever arquivos. Os programadores fazem isso para evitar erros como `FileNotFoundError`, garantindo que a aplicação se comporte de maneira confiável e não trave ao tentar interagir com diretórios.

## Como fazer:
Python fornece maneiras nativas de verificar a existência de um diretório usando os módulos `os` e `pathlib`. Aqui estão exemplos para ambos:

### Usando o módulo `os`
```python
import os

# Especifique o caminho do diretório
dir_path = "/caminho/para/diretorio"

# Verifique se o diretório existe
if os.path.isdir(dir_path):
    print(f"O diretório {dir_path} existe.")
else:
    print(f"O diretório {dir_path} não existe.")
```

### Usando o módulo `pathlib`
```python
from pathlib import Path

# Especifique o caminho do diretório
dir_path = Path("/caminho/para/diretorio")

# Verifique se o diretório existe
if dir_path.is_dir():
    print(f"O diretório {dir_path} existe.")
else:
    print(f"O diretório {dir_path} não existe.")
```

### Bibliotecas de terceiros
Embora a biblioteca padrão do Python seja suficiente para verificar se um diretório existe, bibliotecas como `pathlib2` podem ser alternativas para consistência entre versões do Python ou funcionalidades adicionais.

***Nota:*** Nas últimas versões do Python, `pathlib` é robusto o suficiente para a maioria dos casos de uso, tornando as bibliotecas de terceiros menos necessárias para essa tarefa específica.
