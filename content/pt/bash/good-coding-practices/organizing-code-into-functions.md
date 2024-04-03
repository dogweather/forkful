---
date: 2024-01-26 01:09:04.694547-07:00
description: "Como fazer: Crie uma fun\xE7\xE3o simples em Bash."
lastmod: '2024-03-13T22:44:46.758109-06:00'
model: gpt-4-1106-preview
summary: "Crie uma fun\xE7\xE3o simples em Bash."
title: "Organizando o c\xF3digo em fun\xE7\xF5es"
weight: 18
---

## Como fazer:
Crie uma função simples em Bash:

```Bash
greet() {
  echo "Olá, $1!"
}
```

Use-a chamando a função com um parâmetro:

```Bash
greet "Mundo"  # Saída: Olá, Mundo!
```

Funções podem retornar valores usando `return` para códigos de status numéricos (não para retorno de dados reais):

```Bash
add() {
  return $(($1 + $2))
}

add 3 4
echo $?  # Saída: 7
```

Note que `$?` captura o valor de retorno do último comando, que é o resultado numérico de `add`.

## Aprofundamento
No Bash, funções têm sido uma forma de compartimentar código desde as primeiras versões. Historicamente, o uso de funções está alinhado com princípios de programação estruturada introduzidos nos anos 1960 para melhorar a qualidade do código.

Alternativas às funções incluem a importação de arquivos de script ou o uso de aliases, mas esses não oferecem o mesmo nível de modularidade e reutilização.

Um detalhe de implementação notável no Bash é que funções são cidadãos de primeira classe; elas não têm uma palavra-chave de declaração específica como `function` em outras linguagens, embora `function` seja opcional no Bash para legibilidade. O escopo da função também é interessante — variáveis são globais por padrão, a menos que declaradas como locais, o que pode levar a comportamentos inesperados, se não gerenciados adequadamente.

## Veja Também
- Manual do Bash sobre Funções do Shell: https://www.gnu.org/software/bash/manual/html_node/Shell-Functions.html
- Guia de Scripting Avançado em Bash: https://tldp.org/LDP/abs/html/functions.html
- "Pro Bash Programming: Scripting the GNU/Linux Shell" para conceitos de script de função aprofundados e práticas.
