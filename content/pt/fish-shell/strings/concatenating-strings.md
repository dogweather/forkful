---
date: 2024-01-20 17:34:33.755972-07:00
description: "Como Fazer: Concatenar strings \xE9 uma opera\xE7\xE3o b\xE1sica na\
  \ programa\xE7\xE3o, existindo desde os prim\xF3rdios das linguagens de programa\xE7\
  \xE3o. Cada linguagem tem sua\u2026"
lastmod: '2024-04-05T21:53:47.349886-06:00'
model: gpt-4-1106-preview
summary: "Concatenar strings \xE9 uma opera\xE7\xE3o b\xE1sica na programa\xE7\xE3\
  o, existindo desde os prim\xF3rdios das linguagens de programa\xE7\xE3o."
title: Concatenando strings
weight: 3
---

## Como Fazer:
```Fish Shell
# Concatenar duas strings
set string1 "Hello, "
set string2 "world!"
set result $string1$string2
echo $result
# Saída: Hello, world!

# Usando variáveis e strings literais
set saudacao "Oi, "
echo $saudacao"como vai?"
# Saída: Oi, como vai?

# Concatenar múltiplas strings e variáveis
set animal "gato"
set barulho "miau"
echo "O " $animal " faz " $barulho"."
# Saída: O gato faz miau.
```

## Aprofundando:
Concatenar strings é uma operação básica na programação, existindo desde os primórdios das linguagens de programação. Cada linguagem tem sua forma peculiar de fazer isso, algumas utilizam operadores específicos (como + em Python ou . em PHP), enquanto outras utilizam funções. No Fish Shell, a concatenação é direta: basta escrever as strings e variáveis sequencialmente.

Uma alternativa à concatenação direta é usar comandos como `string join` no Fish, que pode unir elementos com um separador específico.

Detalhes de implementação no Fish: quando se concatena strings na Fish Shell, você está na verdade formando uma nova variável sem usar um operador explícito. É importante apenas garantir que não haja espaços entre as variáveis e as strings, a menos que esse espaço seja intencional.

## Veja Também:
- Documentação oficial do Fish Shell sobre strings: https://fishshell.com/docs/current/index.html#syntax-string
- Tutorial sobre manipulação de strings em Fish: https://fishshell.com/docs/current/tutorial.html#tut_strings
- Fórum de discussão sobre Fish Shell para dúvidas e troca de informações: https://fishshell.com/docs/current/index.html#further-reading
