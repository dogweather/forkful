---
title:                "Concatenando strings"
aliases:
- pt/bash/concatenating-strings.md
date:                  2024-01-20T17:34:00.499087-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenando strings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Concatenar strings é o ato de juntar texto em pedaços, criando uma única sequência contínua. Programadores fazem isso para montar mensagens, gerar caminhos de arquivo, e muitas outras tarefas onde juntar informações em formato de texto é necessário.

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
