---
date: 2024-01-20 17:47:02.285702-07:00
description: "Como Fazer: Historicamente, lidar com strings sempre foi uma necessidade\
  \ em scripts shell, e o Bash oferece v\xE1rios mecanismos para manipula\xE7\xE3\
  o de textos.\u2026"
lastmod: '2024-04-05T21:53:47.088715-06:00'
model: gpt-4-1106-preview
summary: "Historicamente, lidar com strings sempre foi uma necessidade em scripts\
  \ shell, e o Bash oferece v\xE1rios mecanismos para manipula\xE7\xE3o de textos."
title: Descobrindo o comprimento de uma string
weight: 7
---

## Como Fazer:
```Bash
# Usando a variável built-in ${#string}
minha_string="olá, mundo"
echo ${#minha_string} # Saída: 10

# Com o comando expr
comprimento=$(expr length "$minha_string")
echo $comprimento # Saída: 10

# Usando o comando wc junto com echo
comprimento=$(echo -n $minha_string | wc -m)
echo $comprimento # Saída: 10
```

## Mergulho Profundo
Historicamente, lidar com strings sempre foi uma necessidade em scripts shell, e o Bash oferece vários mecanismos para manipulação de textos. A variável `${#string}` é a forma mais direta e eficiente de encontrar o comprimento de uma string no Bash, mas não é a única. O comando `expr` vem dos tempos do Unix e oferece uma abordagem mais portável entre shells. Com `wc -m`, utilizamos a contagem de caracteres do comando `wc`, que embora não seja a maneira mais rápida, pode ser útil em pipelines e scripts mais complexos.

Outra alternativa é usar o `awk`, que é poderoso para processar textos, mas seria como usar um canhão para matar uma mosca no caso simples de contar caracteres. Cada método tem suas vantagens e implicações em termos de portabilidade e performance, e é importante escolher a ferramenta certa para o problema certo.

## Veja Também
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html#Brace-Expansion)
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/index.html)
- [Unix Shell Programming](https://en.wikipedia.org/wiki/Unix_shell#Shell_programming) no Wikipedia para um contexto histórico geral.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/bash) para dúvidas e discussões comunitárias sobre Bash.
