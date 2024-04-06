---
date: 2024-01-20 17:34:00.499087-07:00
description: "Como Fazer: A concatena\xE7\xE3o de strings existe desde os primeiros\
  \ dias da computa\xE7\xE3o, por ser um dos fundamentos para a manipula\xE7\xE3o\
  \ de texto. Em Bash,\u2026"
lastmod: '2024-04-05T21:53:47.090096-06:00'
model: gpt-4-1106-preview
summary: "A concatena\xE7\xE3o de strings existe desde os primeiros dias da computa\xE7\
  \xE3o, por ser um dos fundamentos para a manipula\xE7\xE3o de texto."
title: Concatenando strings
weight: 3
---

## Como Fazer:
```Bash
# Concatenação simples com '+' (evite, não é tão bacana quanto parece)
string1="Olá, "
string2="mundo!"
resultado=$string1$string2
echo $resultado
# Saída: Olá, mundo!

# Usando "{}" para deixar claro onde a string começa e termina
cumprimento="Olá"
nome="João"
mensagem="${cumprimento}, ${nome}!"
echo $mensagem
# Saída: Olá, João!

# Concatenando dentro de um comando echo
echo "Bash " "é " "incrível!"
# Saída: Bash é incrível!
```

## Mergulho Profundo
A concatenação de strings existe desde os primeiros dias da computação, por ser um dos fundamentos para a manipulação de texto. Em Bash, concatenar é diretíssimo – apenas escreva uma variável ao lado da outra. Mas atenção: o '+' entre strings pode parecer tentador, mas não é a abordagem padrão do Bash e pode resultar em confusão.

Podemos usar aspas duplas para expandir as variáveis ou aspas simples se quisermos tratar o valor literalmente. Outro ponto: o uso de "{}" ao redor do nome da variável pode ser útil quando precisamos deixar claro onde a variável termina e o texto literal começa.

Alternativas incluem utilizar o comando `printf` para formatar a saída, ou, para situações mais complexas, pode-se recorrer a ferramentas como `awk` ou `sed` que são mais versáteis para manipulação de texto.

## Veja Também
- Bash String Manipulation Guide: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion
- Bash Programming Introduction HOWTO: http://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
