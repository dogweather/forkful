---
title:                "Interpolando uma string"
html_title:           "Java: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O Quê & Por Quê?

A interpolação de strings é uma maneira de incorporar variáveis dentro de uma string. Os programadores o fazem para criar conteúdo dinâmico e facilitar a leitura e o entendimento do código.

## Como Fazer:

No Bash, você pode interpolar uma string usando o caracter `$`. Veja como é fácil:

```Bash
nome="João"
echo "Olá, $nome"
```
Saída:

```
Olá, João
```
Você também pode usar chaves (`{}`) para interpolar uma string, o que é útil quando a variável faz parte de uma string maior:

```Bash
nome="João"
echo "Olá, ${nome}Smith"
```
Saída:

```
Olá, JoãoSmith
```

## Mergulho Profundo

Desde as suas origens na década de 1970, a interpolação de strings tem sido uma característica comum na maioria das linguagens de programação. No entanto, o Bash aborda de maneira um pouco diferente, com um estilo mais simples e conciso, eliminando a necessidade de funções de concatenação complicadas.

Existem outras maneiras de interpolar uma string em Bash. Por exemplo, você pode usar a sintaxe "-e" para habilitar a interpretação de caractéres de backslash:

```Bash
nome="João"
echo -e "Olá, \n$nome"
```
Saída:

```
Olá, 
João
```

No entanto, este método precisa de cautela, pois pode abrir seu código para vulnerabilidades de injeção de código se a variável não for devidamente sanitizada.

## Veja Também

- Guia de programação Bash: [https://tldp.org/LDP/abs/html/](https://tldp.org/LDP/abs/html/)
- Saiba mais sobre a injeção de código: [https://www.owasp.org/index.php/Code_Injection](https://www.owasp.org/index.php/Code_Injection)