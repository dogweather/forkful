---
title:                "Pesquisando e substituindo texto"
html_title:           "Bash: Pesquisando e substituindo texto"
simple_title:         "Pesquisando e substituindo texto"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Pesquisar e substituir texto é a operação de localizar uma string específica em um volume de texto e substituí-lo por outra. Os programadores fazem isso para modificar dados, corrigir erros ou transformar informações.

## Como Fazer:

```Python
# Aqui temos uma string original
texto = "Eu amo Python!"

# Vamos substituir 'Python' por 'programar'
texto_substituido = texto.replace('Python', 'programar')

# Vamos imprimir a string após a substituição
print(texto_substituido)
```

A saída será:

```Python
Eu amo programar!
```

## Mergulho Profundo:

Descobrir e substituir texto é uma tarefa que tem sido uma prática comum na programação desde os primeiros dias de processamento de texto. Originalmente, foi uma tarefa tediosa que envolveu varrer texto caractere por caractere, mas agora a maioria, se não todas, as linguagens de programação modernas como Python têm funcionalidades integradas para isso.

Alternativas ao uso do método `replace()` em Python incluem o uso de expressões regulares através do módulo `re`, que permite substituições mais complexas e pesquisas com padrões complexos.

A implementação do método `replace()` em Python é muito direta. Ele procura a string velha na string original e a substitui pela nova. Se o número de ocorrências para substituição não for especificado, ele substituirá todas as ocorrências da string velha.

## Veja Também:

- Documentação oficial do Python para a função `replace()`: https://docs.python.org/3/library/stdtypes.html#str.replace
- Tutorial de Python sobre Expressões Regulares (ReGex): https://docs.python.org/3/library/re.html
- Curso do Codecademy sobre manipulação de string em Python: https://www.codecademy.com/learn/learn-python-3/modules/learn-python3-strings