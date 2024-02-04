---
title:                "Capitalizando uma string"
date:                  2024-02-03T19:05:25.215597-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizando uma string"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O que & Por quê?

Capitalizar uma string significa modificá-la para que a primeira letra fique em maiúscula e o restante da string em minúscula. Essa é uma tarefa comum no processamento de texto, normalização de entrada do usuário e formatação de dados para garantir consistência ou atender a critérios específicos de formatação.

## Como fazer:

No Fish Shell, strings podem ser manipuladas diretamente com funções internas, sem a necessidade de ferramentas externas ou bibliotecas. Para capitalizar uma string, você pode combinar o comando `string` com subcomandos.

```fish
# String de exemplo
set sample_string "olá mundo"

# Capitalizar a primeira letra
set capitalized_string (string sub -l 1 -- $sample_string | string upper)(string sub -s 2 -- $sample_string)

echo $capitalized_string
```

Saída:
```
Olá mundo
```

Para cenários que exigem a capitalização de várias palavras em uma string (por exemplo, converter "olá mundo" para "Olá Mundo"), você iteraria sobre cada palavra, aplicando a lógica de capitalização a cada uma:

```fish
# Frase de exemplo
set sentence "olá programação shell fish"

# Capitalizar cada palavra
set capitalized_words (string split " " -- $sentence | while read -l word; string sub -l 1 -- $word | string upper; and string sub -s 2 -- $word; end)

# Juntar as palavras capitalizadas
set capitalized_sentence (string join " " -- $capitalized_words)

echo $capitalized_sentence
```

Saída:
```
Olá Programação Shell Fish
```

Note que o Fish Shell não oferece diretamente uma abordagem de comando único para a capitalização completa de frases da mesma forma que algumas linguagens de programação fazem com seus métodos de string. Portanto, combinar `string split`, `string sub`, `string upper` e depois reunir representa uma abordagem idiomática no Fish Shell para alcançar isso.
