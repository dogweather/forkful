---
title:                "Capitalizando uma string"
date:                  2024-01-19
html_title:           "Bash: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Capitalizar uma string significa transformar as primeiras letras das palavras em maiúsculas. Programadores fazem isso para normalizar dados, melhorar a legibilidade ou atender a requisitos estéticos e de formatação.

## Como Fazer:

```Fish Shell
function capitalize
    for word in $argv
        echo -n (string ucfirst $word) " "
    end
    echo
end

# Exemplo de uso
set phrase "o mundo é vasto e maravilhoso"
capitalize $phrase
```

Saída do exemplo:
```
O Mundo É Vasto E Maravilhoso
```

## Mergulho Profundo:

Antigamente, capitalizar strings não era uma funcionalidade padrão em muitas linguagens de programação e shells – os desenvolvedores tinham que implementar suas próprias funções. Com o Fish Shell, isso mudou. A função `string ucfirst` é prática e direta. Existem alternativas, como usar comandos `awk` ou `sed` em outros shells, mas Fish traz isso de série, sem complicações.

Em termos de implementação, a função `string ucfirst` modifica apenas o primeiro caractere para maiúsculo. Se precisar de mais controle ou quiser capitalizar cada palavra em uma string, você terá que usar um loop ou outra função personalizada, como mostrado acima.

## Veja Também:

- Documentação oficial do comando `string` no Fish: [fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
