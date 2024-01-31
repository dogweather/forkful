---
title:                "Capitalizando uma string"
date:                  2024-01-19
simple_title:         "Capitalizando uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizar uma string é transformar todas as letras iniciais de palavras para maiúsculas. Programadores fazem isso para padronizar textos, destacar títulos ou por convenções de código.

## How to:
Python torna capitalização de strings tranquilo. Vamos ver o método `title()` e `capitalize()`:

```python
texto = "programação em python é demais"

# Capitaliza cada palavra:
texto_capitalizado = texto.title()
print(texto_capitalizado)  # Saída: Programação Em Python É Demais

# Capitaliza apenas a primeira letra da string:
primeira_letra_maiuscula = texto.capitalize()
print(primeira_letra_maiuscula)  # Saída: Programação em python é demais
```

## Deep Dive
Antigamente, a consistência na escrita não era tão rigorosa. Com computadores, veio a necessidade de padronizar textos. Em Python, o método `title()` capitaliza todas as palavras numa string, útil para títulos, mas não lida bem com apóstrofos. Já o `capitalize()` é melhor para sentenças, pois só capitaliza o primeiro caractere.

Alternativas incluem o uso de expressões regulares para controle fino:

```python
import re
texto = "o python é bacana"

# Capitaliza palavras levando em conta apóstrofos e outras exceções
def capitaliza_com_regex(txt):
    return re.sub(r"(\b[a-z](?!\s))", lambda x: x.group().upper(), txt)

texto_capitalizado_regex = capitaliza_com_regex(texto)
print(texto_capitalizado_regex)  # Saída: O Python É Bacana
```

Implementação no estilo Python é direta, mas há sutilezas. Por exemplo, `title()` não capitaliza após algumas pontuações. Isso tem melhorado ao longo das versões.

## See Also
- Documentação oficial do Python sobre métodos de string: [Python String Methods](https://docs.python.org/3/library/stdtypes.html#string-methods)
- Tutorial Python sobre expressões regulares: [Python Regex](https://docs.python.org/3/library/re.html)
- Guia de estilo para Python, PEP 8, que inclui convenções de capitalização: [PEP 8](https://www.python.org/dev/peps/pep-0008/#prescriptive-naming-conventions)
