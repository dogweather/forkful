---
title:                "Utilizando expressões regulares"
date:                  2024-01-19
html_title:           "Bash: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"

category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O que é & Por quê?

Expressões regulares são padrões usados para encontrar correspondências de strings de texto. Programadores usam-nas para validação de dados, busca e substituição de texto complexo, e manipulação de strings com eficiência e precisão.

## Como fazer:

```Python
import re

# Encontrar todos os e-mails
texto = "contato@exemplo.com, suporte@exemplo.br, user@domain.info"
emails = re.findall(r'\b[\w.-]+@[\w.-]+\.\w{2,4}\b', texto)
print(emails)

# Validar um número de telefone brasileiro (formato simples)
telefone = "+55 (21) 90000-0000"
if re.match(r'\+\d{2} \(\d{2}\) \d{5}-\d{4}', telefone):
    print("Telefone válido")
else:
    print("Telefone inválido")

# Substituir espaços múltiplos por um único espaço
texto_com_espacos = "Texto   com  espaços desnecessários."
texto_limpo = re.sub(r'\s+', ' ', texto_com_espacos)
print(texto_limpo)
```

Saída:

```
['contato@exemplo.com', 'suporte@exemplo.br', 'user@domain.info']
Telefone válido
Texto com espaços desnecessários.
```

## Mergulho Profundo

Expressões regulares surgiram nos anos 1950 com o matemático Stephen Cole Kleene. Alternativas a expressões regulares incluem métodos de string específicos como `str.find()` ou `str.replace()`, mas eles não têm a mesma potência ou flexibilidade. Sob o capô, expressões regulares utilizam um motor de busca que pode ser do tipo "greedy" (guloso) ou "non-greedy" (não-guloso), sendo o primeiro o padrão em muitas implementações, o que significa que busca a maior correspondência possível.

## Veja Também

- Documentação oficial do Python sobre expressões regulares: https://docs.python.org/3/library/re.html
- Tutorial interativo de expressões regulares: https://regexr.com/
- Artigo sobre a teoria computacional por trás das expressões regulares: https://www.geeksforgeeks.org/regular-expressions-in-python/
