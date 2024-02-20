---
date: 2024-01-26 04:13:54.470592-07:00
description: "REPL, ou Loop de Ler-Avaliar-Imprimir, \xE9 um ambiente de programa\xE7\
  \xE3o interativo que recebe entradas \xFAnicas do usu\xE1rio, executa-as e retorna\
  \ o resultado.\u2026"
lastmod: 2024-02-19 22:05:06.074985
model: gpt-4-0125-preview
summary: "REPL, ou Loop de Ler-Avaliar-Imprimir, \xE9 um ambiente de programa\xE7\xE3\
  o interativo que recebe entradas \xFAnicas do usu\xE1rio, executa-as e retorna o\
  \ resultado.\u2026"
title: Usando um shell interativo (REPL)
---

{{< edit_this_page >}}

## O Quê & Por Quê?
REPL, ou Loop de Ler-Avaliar-Imprimir, é um ambiente de programação interativo que recebe entradas únicas do usuário, executa-as e retorna o resultado. Programadores o utilizam para obter feedback imediato, depuração e experimentação rápida com conceitos de codificação sem o excesso de compilar e executar um programa completo.

## Como fazer:
No Fish, o shell interativo é o modo padrão quando você o inicia. Veja como ele funciona na prática:

```Fish Shell
> set color azul
> echo "O céu é $color"
O céu é azul
```

Você também pode executar funções incorporadas e brincar com substituições de comandos:

```Fish Shell
> function animar
      echo "Vamos lá Fish $argv!"
  end
> animar Programadores
Vamos lá Fish Programadores!
```

Não apenas definindo funções, você pode executar trechos de código instantaneamente e ver a saída imediatamente:

```Fish Shell
> math "40 / 2"
20
```

## Mergulho Profundo
O conceito de REPLs remonta à linguagem de programação Lisp na década de 1960. Esta forma de programação interativa estabeleceu o padrão para ambientes como o `ipython` do Python e o `irb` do Ruby. Fish continua a tendência com foco na facilidade de uso e uso interativo.

Fish difere de outros shells como Bash pelo fato de ser projetado com a interatividade em mente desde o início. Ele oferece realce de sintaxe, sugestões automáticas e complementos de tabulação que o tornam poderoso para uso em um fluxo de trabalho estilo REPL. Melhor ainda, seus comandos são lembrados e pesquisáveis, tornando os testes repetidos uma brisa.

Alternativas ao REPL do Fish poderiam ser `bash` ou `zsh` quando emparelhados com extensões como `bash-completion` ou `oh-my-zsh`, mas Fish tende a oferecer uma experiência mais rica imediatamente.

## Veja Também:
- Documentação do Fish: https://fishshell.com/docs/current/index.html
- Uma comparação interessante entre Fish e outros shells: https://www.slant.co/versus/2209/3686/~fish_vs_bash
- Um mergulho mais profundo nos REPLs: https://en.wikipedia.org/wiki/Read–eval–print_loop
- Programação interativa em Lisp, uma olhada histórica: http://www.paulgraham.com/ilisp.html
