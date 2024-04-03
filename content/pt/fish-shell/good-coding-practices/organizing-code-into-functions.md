---
changelog:
- 2024-01-28, dogweather, reviewed and added links
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:01:27.474205-07:00
description: "Organizar o c\xF3digo em fun\xE7\xF5es \xE9 sobre agrupar peda\xE7os\
  \ de script para realizar tarefas espec\xEDficas. Fazemos isso porque torna o c\xF3\
  digo mais f\xE1cil de ler,\u2026"
lastmod: '2024-03-13T22:44:47.011554-06:00'
model: gpt-4-0125-preview
summary: "Organizar o c\xF3digo em fun\xE7\xF5es \xE9 sobre agrupar peda\xE7os de\
  \ script para realizar tarefas espec\xEDficas."
title: "Organizando c\xF3digo em fun\xE7\xF5es"
weight: 18
---

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

Agora, vamos fazer com que cumprimente um usuário:

```fish
function greet
    set user (whoami)
    echo "Ei, $user!"
end

greet
```

Saída:
```
Ei, seu_nome_de_usuário!
```

Para salvar isso entre sessões, use `funcsave greet`.

## Mergulho Profundo
Funções no Fish Shell são como mini-scripts — você pode colocar praticamente qualquer coisa nelas. Historicamente, o conceito de funções em scripts shell salvou incontáveis horas de digitação e depuração repetitivas. Diferente de linguagens de programação como Python, as funções Shell são mais sobre conveniência do que estrutura.

Algumas shells, como o Bash, usam `function` ou apenas chaves diretas. Fish se atém a `function ... end` — claro e legível. Dentro das funções Fish, você tem todos os recursos: parâmetros, variáveis locais com `set -l`, e você pode até definir uma função dentro de outra função.

Você não precisará de um valor de `return` porque o Fish não se concentra nisso; a saída da sua função é o seu retorno. E se você quiser funções persistentes disponíveis para sessões futuras, lembre-se de `funcsave`.

## Veja Também
- O tutorial do fish sobre funções: [https://fishshell.com/docs/current/tutorial.html#tut_functions](https://fishshell.com/docs/current/tutorial.html#functions)

### Comandos de função
- [function](https://fishshell.com/docs/current/cmds/function.html) — Crie uma função
- [functions](https://fishshell.com/docs/current/cmds/functions.html) — Imprima ou apague funções
- [funcsave](https://fishshell.com/docs/current/cmds/funcsave.html) — Salve a definição de uma função no diretório de autoload do usuário
- [funced](https://fishshell.com/docs/current/cmds/funced.html) — Edite uma função interativamente
