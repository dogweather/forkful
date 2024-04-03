---
date: 2024-01-26 03:38:50.572644-07:00
description: "Como fazer: Fish tem uma m\xE1gica embutida para esse tipo de tarefa.\
  \ Use a fun\xE7\xE3o `string` sem suar a camisa. Confira estes feiti\xE7os."
lastmod: '2024-03-13T22:44:46.992398-06:00'
model: gpt-4-0125-preview
summary: "Fish tem uma m\xE1gica embutida para esse tipo de tarefa."
title: Removendo aspas de uma string
weight: 9
---

## Como fazer:
Fish tem uma mágica embutida para esse tipo de tarefa. Use a função `string` sem suar a camisa. Confira estes feitiços:

```fish
# Exemplo com aspas simples
set quoted "'Olá, Mundo!'"
set unquoted (string trim --chars \"\'\" $quoted)
echo $unquoted # Saída: Olá, Mundo!

# Mesmo esquema com aspas duplas
set double_quoted "\"Olá, Universo!\""
set unquoted (string trim --chars \"\'\" $double_quoted)
echo $unquoted # Saída: Olá, Universo!
```

## Mergulho Profundo
Nos primórdios da linha de comando, você lutaria com `sed` ou `awk` para remover aspas; uma verdadeira confusão de barras invertidas e flags enigmáticas. A função `string` do Fish é de uma era mais nova, tornando o código mais limpo e intuitivo.

Alternativas em outros shells ainda podem depender dessas ferramentas antigas ou podem usar seus próprios métodos integrados como a expansão de parâmetros do bash ou os modificadores do zsh.

A função `string` vai além de aparar aspas. É um canivete suíço para operações com strings no Fish. Com `string`, você pode cortar, dividir, unir ou até mesmo combinar strings com expressões regulares diretamente no seu terminal.

## Veja Também
Mergulhe mais fundo em `string` com a ajuda da documentação oficial:
- [Documentação de String do Fish Shell](https://fishshell.com/docs/current/commands.html#string)

Para nostalgia ou ao escrever scripts com shells mais tradicionais, confira:
- [Guia Sed & Awk](https://www.grymoire.com/Unix/Sed.html)
- [Expansão de Parâmetros do Bash](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
