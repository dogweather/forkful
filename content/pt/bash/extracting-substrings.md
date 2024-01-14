---
title:    "Bash: Extraindo Substrings"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por que

Em programação, muitas vezes precisamos manipular strings de texto. Uma operação muito comum é extrair uma parte específica de uma string, também conhecida como substring. Isso pode ser útil para filtrar dados, formatar saídas ou realizar outras operações. Aprender a extrair substrings é uma habilidade útil para qualquer programador Bash.

## Como fazer

Extração de substrings é uma tarefa relativamente simples em Bash. Utilizamos o operador de subshell "$" e as chaves "{ }" para indicar o início e o fim da substring desejada. Podemos também adicionar um número após o operador "$", que indica a posição do caractere na string. Por exemplo, se quisermos extrair a segunda letra de uma string, poderíamos usar o código:

```
Bash ${texto:1:1}
```
Nesse exemplo, "texto" é a variável que contém a string desejada e o número "1" é a posição da letra que queremos extrair. A saída desse comando seria a segunda letra da string. 

Podemos também utilizar o operador ":" apenas com o parâmetro de início, o que indica ao Bash que queremos pegar todos os caracteres a partir dessa posição até o fim da string. Por exemplo:

```
Bash ${texto:5}
```
Nesse caso, a saída seria todos os caracteres da posição 5 até o fim da string.

## Mergulho Profundo

Além de extrair substrings de uma string, também podemos utilizar outros recursos para manipular e modificar strings. Por exemplo, podemos utilizar o operador "tr" para substituir um caractere específico por outro. Ou utilizar o comando "sed" para buscar e substituir padrões de texto em uma string. 

Também é possível extrair substrings de arquivos, utilizando os comandos "cat" e "grep" para encontrar uma string específica e depois extrair a substring desejada. 

## Veja também

- [Tutorial de Bash - Extrair Substrings](https://www.digitalocean.com/community/tutorials/how-to-index-and-slice-strings-in-bash)
- [Comandos de manipulação de strings](https://www.gnu.org/software/bash/manual/html_node/String-Manipulation.html)
- [Extraindo substrings de arquivos](https://linuxhint.com/extract_substring_bash/)
- [Referência do Bash](https://linux.die.net/man/1/bash)