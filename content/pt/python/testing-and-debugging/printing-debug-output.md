---
title:                "Exibindo saídas de depuração"
aliases:
- pt/python/printing-debug-output.md
date:                  2024-01-20T17:53:25.222977-07:00
model:                 gpt-4-1106-preview
simple_title:         "Exibindo saídas de depuração"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

A impressão de saídas de depuração permite que programadores rastreiem e entendam o que está acontecendo dentro de seu código ao executá-lo. Fazemos isso para identificar e corrigir bugs, otimizar o desempenho e assegurar que a lógica do programa está correta.

## Como fazer:

Para imprimir uma saída simples no Python, utilizamos a função `print()`. Aqui está um exemplo de como usá-la para depurar:

```Python
def somar(a, b):
    resultado = a + b
    print(f"Debug: a soma de {a} e {b} é {resultado}")
    return resultado

soma = somar(3, 4)
```

Saída:

```
Debug: a soma de 3 e 4 é 7
```

Podemos também logar diferentes níveis de depuração usando o módulo `logging`:

```Python
import logging

logging.basicConfig(level=logging.DEBUG)
logging.debug('Isto é uma mensagem de debug.')

```

Saída:

```
DEBUG:root:Isto é uma mensagem de debug.
```

## Aprofundando:

Vamos nos aprofundar um pouco na história e detalhes de implementação. Originalmente, a impressão de saídas de depuração era feita com chamadas simples à função `print()`, que enviavam o texto ao stdout (saída padrão). Com o tempo, ferramentas mais avançadas como o módulo `logging` foram criadas, oferecendo níveis de depuração (DEBUG, INFO, WARNING, ERROR, CRITICAL) que possibilitam um controle mais refinado sobre o que é registrado.

Além disso, em ambientes de produção, o `logging` permite que mensagens de depuração sejam escritas em arquivos de log ao invés da saída padrão, o que facilita a análise posterior de incidentes.

Quanto à implementação, o uso do `logging` é preferível ao `print()` por ser mais flexível e poderoso. Vale ressaltar que mensagens excessivas de depuração podem impactar a performance do programa e devem ser usadas com cautela em um ambiente de produção.

## Veja Também:

- Documentação oficial do Python sobre logging: https://docs.python.org/3/library/logging.html
- Tutorial interativo de Python: https://www.learnpython.org/ 
- Perguntas e respostas sobre Python (Stack Overflow em português): https://pt.stackoverflow.com/questions/tagged/python
