---
title:    "Bash: Encontrando o comprimento de uma string."
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por que

A determinação do comprimento de uma string é uma tarefa muito comum em programas Bash. Saber o tamanho de uma string é útil para diversos fins, como a validação de entradas de usuário, a manipulação de dados e a execução de operações em strings específicas. Neste artigo, vamos explorar como encontrar o comprimento de uma string em Bash.

## Como Fazer

Para encontrar o comprimento de uma string em Bash, podemos usar o parâmetro especial `#` seguido pela variável na qual a string está armazenada. Por exemplo, para encontrar o comprimento da string "Olá, mundo!", podemos usar o seguinte comando:

```Bash
echo ${#string} 
```

Isso retornará o valor 12, indicando que a string tem 12 caracteres. Podemos então usar esse valor em nossos programas para executar diferentes operações com a string.

## Profundidade

Ao usar o parâmetro `#` para encontrar o comprimento de uma string, é importante ter em mente que ele conta o número de caracteres da string, incluindo espaços e caracteres especiais. Por exemplo, se usarmos esse método para encontrar o comprimento da string "hello world!", receberemos o valor 12, mesmo que haja um espaço entre "hello" e "world". Portanto, é importante ter cuidado ao usar esse método e verificar se ele se adequa às suas necessidades.

Também é possível usar o comando `expr length` para encontrar o comprimento de uma string, como mostrado no exemplo a seguir:

```Bash
expr length $string
```

Este comando retornará o valor exato do comprimento da string, sem incluir espaços ou caracteres especiais.

## Veja Também

Você pode encontrar mais informações e exemplos sobre a determinação do comprimento de strings em Bash nos seguintes links:

- https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
- https://bash.cyberciti.biz/guide/Length_of_a_String
- https://linuxize.com/post/bash-string-length/