---
aliases:
- /pt/python/searching-and-replacing-text/
date: 2024-01-20 17:59:00.053528-07:00
description: "Buscar e substituir texto \xE9 o processo de localizar sequ\xEAncias\
  \ de caracteres dentro de strings e troc\xE1-las por outras. Programadores realizam\
  \ essa tarefa\u2026"
lastmod: 2024-02-18 23:08:57.746635
model: gpt-4-1106-preview
summary: "Buscar e substituir texto \xE9 o processo de localizar sequ\xEAncias de\
  \ caracteres dentro de strings e troc\xE1-las por outras. Programadores realizam\
  \ essa tarefa\u2026"
title: Pesquisando e substituindo texto
---

{{< edit_this_page >}}

## O Que & Porquê?
Buscar e substituir texto é o processo de localizar sequências de caracteres dentro de strings e trocá-las por outras. Programadores realizam essa tarefa para corrigir erros, atualizar informações ou automatizar a edição de código e dados.

## Como fazer:
Aqui estão alguns exemplos de como buscar e substituir texto em Python. Comentários no código ajudam a explicar o passo a passo.

```Python
# Importando o módulo 're' para expressões regulares
import re

texto_original = "Olá, nome. Bem-vindo ao mundo da programação!"

# Substituindo 'nome' por 'João'
texto_modificado = texto_original.replace('nome', 'João')
print(texto_modificado)  # Saída: Olá, João. Bem-vindo ao mundo da programação!

# Usando expressões regulares para substituir qualquer palavra seguida de 'programação'
texto_regex = re.sub(r'\b\w+\b(?= programação)', 'Python', texto_original)
print(texto_regex)  # Saída: Olá, nome. Bem-vindo ao mundo da Python!

# Substituição insensível a maiúsculas e minúsculas
texto_case_insensitive = re.sub(r'olá', 'Oi', texto_original, flags=re.IGNORECASE)
print(texto_case_insensitive)  # Saída: Oi, nome. Bem-vindo ao mundo da programação!
```

## Mergulho Profundo
Buscar e substituir texto é uma necessidade antiga na computação, tendo suas raízes em editores de texto como ed e vi. Python oferece a função `replace()` para substituições simples e o módulo `re` para expressões regulares, que possibilita uma busca e substituição mais avançada com padrões complexos.

Alternativas incluem o uso de ferramentas como sed e awk em ambientes Unix e módulos de Python como `string` e bibliotecas de terceiros, por exemplo, `regex`.

Em relação a implementações, a função `replace()` é direta e realiza uma busca linear, o que é eficiente para textos pequenos. Já as expressões regulares oferecem flexibilidade, mas podem ser mais lentas e complicadas.

## Veja Também
Aprofunde seus conhecimentos com estas fontes:

- [Documentação oficial do Python sobre expressões regulares (re)](https://docs.python.org/3/library/re.html)
- [Documentação oficial da função built-in replace()](https://docs.python.org/3/library/stdtypes.html#str.replace)
- [Tutorial interativo de expressões regulares](https://regexone.com/)
- [Livro "Mastering Regular Expressions" de Jeffrey E.F. Friedl](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)
