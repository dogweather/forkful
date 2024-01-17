---
title:                "Convertendo uma string para minúsculas"
html_title:           "Bash: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O que é e porquê?
Converter uma string para letras minúsculas significa transformar todas as letras em uma palavra ou frase para sua forma menor correspondente. Programadores fazem isso por vários motivos, incluindo padronização de entrada de dados e comparação de strings sem diferenciar entre maiúsculas e minúsculas.

## Como fazer:
Existem várias maneiras de converter uma string para letras minúsculas usando Bash. Uma maneira simples é usar o comando `tr` para substituir as letras maiúsculas por minúsculas em uma variável:

```
vogal='A'               
echo "Vogal em minúsculo: $(echo ${vogal} | tr '[A-Z]' '[a-z]')"
# Output: Vogal em minúsculo: a
```

Outra opção é usar o operador `^^` para converter toda a string em maiúsculas primeiro e depois substituir pelas letras minúsculas:

```
nome='JOAO'
echo "Nome em minúsculo: ${nome,}"
# Output: Nome em minúsculo: joao
```

Você também pode usar o comando `sed` para substituir caracteres maiúsculos por minúsculos em uma variável:

```
frase='O Inverno Está Chegando'
echo "Frase em minúsculo: $(echo ${frase} | sed 's/[A-Z]/\L&/g')"
# Output: Frase em minúsculo: o inverno está chegando
```

## Mergulho Profundo:
A conversão de strings para letras minúsculas é algo comumente usado em programação e é uma funcionalidade básica da linguagem Bash. Além das opções mencionadas acima, também é possível utilizar o comando `awk` e funções internas do Bash para realizar a conversão. Além disso, é importante ter em mente que nem todas as linguagens de programação possuem esse recurso integrado, portanto, pode ser necessário implementá-lo manualmente em alguns casos.

## Veja também:
- [Conversão de string para letras minúsculas em Bash com opções adicionais](https://www.baeldung.com/linux/bash-convert-string-to-lowercase)
- [Comandos sed e awk em Bash](https://www.tecmint.com/sed-awk-cheatsheet-with-examples/)