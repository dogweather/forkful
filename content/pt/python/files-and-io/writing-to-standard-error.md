---
title:                "Escrevendo para o erro padrão"
aliases:
- /pt/python/writing-to-standard-error.md
date:                  2024-02-03T19:34:16.637262-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escrevendo para o erro padrão"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Quê & Por Quê?
Escrever para o erro padrão em Python trata-se de direcionar as mensagens de erro ou diagnósticos do seu programa para o fluxo de erro (`stderr`), separado da saída padrão (`stdout`). Os programadores fazem isso para diferenciar as saídas normais do programa das mensagens de erro, facilitando a depuração e análise de logs.

## Como fazer:
### Usando `sys.stderr`
O módulo interno `sys` do Python permite escrever explicitamente para `stderr`. Esta abordagem é direta para mensagens de erro simples ou diagnósticos.

```python
import sys

sys.stderr.write('Erro: Algo deu errado.\n')
```
Saída de exemplo (para stderr):
```
Erro: Algo deu errado.
```

### Usando a função `print`
A função `print` do Python pode redirecionar sua saída para `stderr` especificando o parâmetro `file`. Este método é útil para aproveitar a facilidade de uso do `print` enquanto lida com mensagens de erro.
```python
from sys import stderr

print('Erro: Falha no módulo.', file=stderr)
```
Saída de exemplo (para stderr):
```
Erro: Falha no módulo.
```

### Usando o módulo `logging`
Para uma solução mais abrangente, o módulo de `logging` do Python pode direcionar mensagens para `stderr` e muito mais, como escrever para um arquivo ou personalizar o formato da mensagem. Este método é melhor para aplicações que requerem diferentes níveis de registro, formatação de mensagens ou destinos.
```python
import logging

logging.basicConfig(level=logging.WARNING)
logger = logging.getLogger(__name__)

logger.error('Erro: Falha na conexão com a base de dados.')
```
Saída de exemplo (para stderr):
```
ERROR:__main__:Erro: Falha na conexão com a base de dados.
```

### Bibliotecas de terceiros: `loguru`
`loguru` é uma biblioteca de terceiros popular que simplifica o registro de logs em aplicações Python. Ela direciona automaticamente os erros para `stderr`, entre outras funcionalidades.

Para usar o `loguru`, primeiro instale-o via pip:
```shell
pip install loguru
```

Em seguida, incorpore-o ao seu script Python da seguinte maneira:
```python
from loguru import logger

logger.error('Erro: Falha ao abrir arquivo.')
```
Saída de exemplo (para stderr):
```
2023-04-05 12:00:00.000 | ERROR    | __main__:<module>:6 - Erro: Falha ao abrir arquivo.
```
