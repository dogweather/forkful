---
date: 2024-01-20 17:41:43.307365-07:00
description: "Apagar caracteres que correspondem a um padr\xE3o s\xE3o t\xE9cnicas\
  \ usadas para limpar strings - eliminar o que n\xE3o \xE9 necess\xE1rio ou formatar\
  \ texto. Programadores\u2026"
lastmod: '2024-03-13T22:44:46.735365-06:00'
model: gpt-4-1106-preview
summary: "Apagar caracteres que correspondem a um padr\xE3o s\xE3o t\xE9cnicas usadas\
  \ para limpar strings - eliminar o que n\xE3o \xE9 necess\xE1rio ou formatar texto."
title: "Excluindo caracteres que correspondem a um padr\xE3o"
weight: 5
---

## O Que & Por Quê?
Apagar caracteres que correspondem a um padrão são técnicas usadas para limpar strings - eliminar o que não é necessário ou formatar texto. Programadores fazem isso para simplificar o processamento de dados ou prepará-los para exibição, armazenamento ou outra operação de manipulação de texto.

## Como Fazer:
```Bash
# Remover todos os dígitos de uma string
echo "Ano2023" | tr -d '0-9'
# Saída: Ano

# Deletar todos os caracteres exceto letras minúsculas
echo "Bash_Version_5.1.4" | tr -dc 'a-z\n'
# Saída: ashersion

# Substituir espaços por novas linhas e depois remover linhas vazias
echo "Bash é divertido" | tr ' ' '\n' | sed '/^$/d'
# Saída:
# Bash
# é
# divertido
```

## Aprofundando:
A eliminação de caracteres que correspondem a padrões específicos não é algo novo. No Unix, ferramentas como `sed`, `awk`, e `tr` vêm permitindo isso por décadas. Existem várias formas de fazer:

- `tr`: traduz ou deleta caracteres.
- `sed`: editor de stream que pode ser usado para remover caracteres usando expressões regulares.
- `awk`: linguagem de programação voltada para manipulação de texto.

O `tr` é geralmente mais rápido, mas é limitado a caractere por caractere. O `sed` e `awk` oferecem mais flexibilidade com expressões regulares, podendo tratar padrões mais complexos.

Cada uma dessas ferramentas tem implicações em termos de desempenho e complexidade, e a escolha depende do caso de uso específico e da familiaridade do usuário com a ferramenta.

## Veja Também:
- Bash Reference Manual: https://www.gnu.org/software/bash/manual/
- Regular Expressions Info – tr command: https://www.regular-expressions.info/posixbrackets.html
- GNU Sed Manual: https://www.gnu.org/software/sed/manual/sed.html
- AWK Manual: https://www.gnu.org/software/gawk/manual/gawk.html
