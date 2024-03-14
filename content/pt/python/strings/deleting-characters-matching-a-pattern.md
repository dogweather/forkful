---
date: 2024-01-20 17:42:47.654872-07:00
description: "Deletar caracteres que correspondem a um padr\xE3o \xE9 uma forma de\
  \ filtrar strings, preservando s\xF3 o que \xE9 relevante para o seu contexto. Programadores\
  \ fazem\u2026"
lastmod: '2024-03-13T22:44:46.137836-06:00'
model: gpt-4-1106-preview
summary: "Deletar caracteres que correspondem a um padr\xE3o \xE9 uma forma de filtrar\
  \ strings, preservando s\xF3 o que \xE9 relevante para o seu contexto. Programadores\
  \ fazem\u2026"
title: "Excluindo caracteres que correspondem a um padr\xE3o"
---

{{< edit_this_page >}}

## O Que & Por Que?
Deletar caracteres que correspondem a um padrão é uma forma de filtrar strings, preservando só o que é relevante para o seu contexto. Programadores fazem isso para limpar dados, validar entradas ou simplificar a manipulação de strings.

## Como Fazer:
```Python
import re

# Exemplo 1: Deletar dígitos de uma string
texto = "Ano2023, Mes03"
padrao = r'\d+'  # dígitos
texto_limpo = re.sub(padrao, '', texto)
print(texto_limpo)  # Saída: Ano, Mes

# Exemplo 2: Remover caracteres especiais, exceto espaço
descricao = "Produto#7 - @Excelente!"
padrao = r'[^\w\s]'  # não-palavras e não-espaços
descricao_limpa = re.sub(padrao, '', descricao)
print(descricao_limpa)  # Saída: Produto7  Excelente
```

## Mergulho Profundo:
Deletar caracteres específicos de uma string é uma operação comum que remonta às primeiras ferramentas de processamento de texto. No Python, usamos regular expressions (regex), disponíveis no módulo `re`, para definir padrões de caracteres a serem removidos.

- **Contexto histórico:** O uso de expressões regulares começou nos anos 1950. O Python as adotou na sua biblioteca padrão, seguindo a tradição de outras linguagens de programação.
  
- **Alternativas:** Além de `re.sub()`, é possível usar list comprehensions ou funções como `str.replace()` para remover caracteres, mas essas abordagens são menos flexíveis para padrões complexos.

- **Detalhes de implementação:** Ao compilar o padrão com `re.compile()` antes de usá-lo pode-se melhorar a performance se o mesmo regex for utilizado várias vezes.

## Veja Também:
- Documentação do módulo `re` do Python: https://docs.python.org/3/library/re.html
- Tutorial de expressões regulares do Python: https://docs.python.org/3/howto/regex.html
- Artigo sobre a história das expressões regulares: https://dl.acm.org/doi/10.1145/363347.363387
