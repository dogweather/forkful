---
title:                "Organizando o código em funções"
date:                  2024-01-26T01:09:55.816776-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizando o código em funções"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## O Quê & Por Quê?
Organizar o código em funções é sobre agrupar pedaços de script para executar tarefas específicas. Fazemos isso porque torna o código mais fácil de ler, testar e reutilizar — ninguém quer navegar por um pântano de spaghetti de código.

## Como fazer:
No Fish, você escreve uma função com a palavra-chave `function`, dá um nome a ela e termina com `end`. Aqui está uma simples:

```fish
function hello
    echo "Olá, Mundo!"
end

hello
```

Saída:
```
Olá, Mundo!
```

Agora, vamos fazer com que ela cumprimente um usuário:

```fish
function greet
    set user (whoami)
    echo "Ei lá, $user!"
end

greet
```

Saída:
```
Ei lá, seu_nome_de_usuário!
```

Para salvá-la entre sessões, use `funcsave greet`.

## Mergulho Profundo
Funções no Fish Shell são como mini-scripts — você pode colocar praticamente qualquer coisa lá dentro. Historicamente, o conceito de funções em scripts shell salvou incontáveis horas de digitação e depuração repetitivas. Ao contrário de linguagens de programação como Python, as funções Shell são mais sobre conveniência do que estrutura.

Alguns shells, como o Bash, usam `function` ou apenas chaves diretas. Fish mantém-se com `function ... end` — claro e legível. Dentro das funções Fish, você tem todos os recursos: parâmetros, variáveis locais com `set -l`, e você pode até definir uma função dentro de outra.

Você não precisará de um valor de `return` porque Fish não é grande nisso; a saída da sua função é o seu retorno. E, se você quiser funções persistentes disponíveis para sessões futuras, lembre-se de `funcsave`.

## Veja Também
- O tutorial do fish sobre funções: https://fishshell.com/docs/current/tutorial.html#tut_functions
- A documentação do fish para `function`: https://fishshell.com/docs/current/cmds/function.html
- Um guia extensivo sobre como escrever funções em fish: https://fishshell.com/docs/current/index.html#syntax-function
