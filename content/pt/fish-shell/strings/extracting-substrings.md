---
date: 2024-01-20 17:45:31.716514-07:00
description: "Como Fazer: A ideia de trabalhar com substrings remonta aos prim\xF3\
  rdios da programa\xE7\xE3o. Na \xE9poca em que tudo era bits e bytes, j\xE1 era\
  \ essencial manipular\u2026"
lastmod: '2024-04-05T21:53:47.346309-06:00'
model: gpt-4-1106-preview
summary: "A ideia de trabalhar com substrings remonta aos prim\xF3rdios da programa\xE7\
  \xE3o."
title: Extraindo substrings
weight: 6
---

## Como Fazer:
```Fish Shell
# Extraindo substrings com string sub
set exemplo "PeixeBacana123"

# Extraindo os primeiros 5 caracteres
echo (string sub -l 5 $exemplo) # Saída: Peixe

# Extraindo caracteres do índice 6 ao 10
echo (string sub -s 6 -l 5 $exemplo) # Saída: Bacan

# Extraindo tudo após o 10° caractere
echo (string sub -s 11 $exemplo) # Saída: 123

# Extraindo uma substring quando você conhece os caracteres de inicio e fim
echo (string sub -r 2-5 $exemplo) # Saída: eixe
```

## Mergulho Profundo
A ideia de trabalhar com substrings remonta aos primórdios da programação. Na época em que tudo era bits e bytes, já era essencial manipular partes de cadeias de caracteres. No Fish Shell, `string sub` é uma função incorporada versátil e poderosa para lidar com essa tarefa, o que simplifica a operação que em outras shells muitas vezes precisaria de ferramentas externas como `cut`, `awk` ou `sed`.

Outras shells, como Bash, usam sintaxe dentro de parênteses ou operadores para trabalhar com substrings, mas Fish opta por manter as coisas simples e claras com comandos legíveis. Além disso, ao contrário de outras shells que indexam a partir de 0, Fish Shell começa a indexar do 1, o que é intuitivamente mais fácil para muitos, porque é assim que contamos naturalmente.

Manusear substrings no Fish Shell pode ser mais intuitivo e menos propenso a erros para programadores iniciantes e veteranos. A sintaxe clara, o indexamento baseado em 1 e as funções integradas tornam o trabalho com strings um processo suave.

## Veja Também
- Documentação oficial do comando `string`: https://fishshell.com/docs/current/cmds/string.html
- Tutorial sobre manipulação de strings no Fish: https://fishshell.com/docs/current/tutorial.html#tut_strings
- Stack Overflow para perguntas específicas sobre a programação em Fish Shell: https://stackoverflow.com/questions/tagged/fish
