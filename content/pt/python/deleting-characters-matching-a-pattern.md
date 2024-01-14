---
title:    "Python: Excluindo caracteres que correspondem a um padrão"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Por que Deletar Caracteres Correspondentes a um Padrão
Deletar caracteres correspondentes a um padrão é útil ao lidar com strings ou textos que contenham informações desnecessárias ou indesejadas. Isso pode ajudar a tornar seus dados mais limpos e fáceis de trabalhar.

## Como Fazer
Usando o Python, podemos usar a função `re.sub()` para encontrar e substituir padrões em uma string. Aqui está um exemplo de código que remove todas as vogais de uma string:

```Python
import re

string = "Essa é uma string com muitas vogais"

padrao = "[aeiou]"

nova_string = re.sub(padrao, "", string)

print(nova_string) # Ss  é  strng cm mts vgls
```

Neste exemplo, usamos o módulo `re` para acessar a função `sub()`, que nos permite substituir caracteres correspondentes ao padrão por uma string vazia. Também usamos uma expressão regular `[aeiou]` para representar todas as vogais.

## Mergulho Profundo
Além de usar a função `sub()`, o módulo `re` também possui outras funções úteis que permitem a manipulação avançada de strings. Por exemplo, podemos usar a função `findall()` para encontrar todas as correspondências de um padrão em uma string e retorná-las em uma lista. Além disso, também podemos usar grupos de captura para agrupar partes da string correspondentes ao padrão.

É importante notar que o uso de expressões regulares pode ser um pouco complexo, mas com a prática, você poderá dominar essa habilidade útil no seu código Python.

## Veja Também
- [Documentação oficial do módulo re em Python](https://docs.python.org/3/library/re.html)
- [Tutorial de Expressões Regulares em Python](https://realpython.com/regex-python/)
- [Exercícios práticos de Expressões Regulares em Python](https://regexone.com/lesson/introduction_abcs)