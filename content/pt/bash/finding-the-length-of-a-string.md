---
title:    "Bash: Encontrando o comprimento de uma string."
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Encontrar o comprimento de uma string é uma habilidade muito útil ao programar em Bash. Isso permite que você obtenha informações sobre o tamanho de uma determinada palavra ou frase, o que pode ser útil em diversas situações.

## Como Fazer

Para encontrar o comprimento de uma string em Bash, você pode usar o comando `expr length`, seguido da string sobre a qual deseja obter o tamanho. Por exemplo:

```Bash
string="Bash é incrível"
echo "O comprimento da string é: $(expr length "$string")"
```

Isso produzirá a saída: `O comprimento da string é: 15`, indicando que a string "Bash é incrível" tem 15 caracteres.

Você também pode usar parênteses duplos `(( ))` para encontrar o comprimento de uma string sem usar o comando `expr length`. Por exemplo:

```Bash
string="Markdown é legal"
echo "O comprimento da string é: ${#string}"
```

Isso também produzirá a saída: `O comprimento da string é: 16`.

## Deep Dive

Em Bash, uma string é tratada como um array de caracteres. Isso significa que cada caractere ocupa uma posição no array e o comprimento total da string é igual ao número de posições no array.

Além disso, o comando `expr length` conta o número de caracteres e espaços em branco na string. Por exemplo, se tivermos a string "Bash é incrível", o comando `expr length` retornará 15, mesmo que tenhamos um espaço em branco entre as palavras.

Uma maneira de evitar isso é usar o parêntese duplo `(( ))`, que conta apenas os caracteres na string, ignorando os espaços em branco.

## Veja Também

- [Documentação do Bash](https://www.gnu.org/software/bash/)
- [Tutorial do Bash para iniciantes](https://linuxize.com/post/bash-scripting-tutorial-for-beginners/#4-working-with-strings)
- [Usando o operador de atribuição de parâmetros em Bash](https://www.gnu.org/software/bash/manual/bash.html#index-_005fParameter-Expansion)