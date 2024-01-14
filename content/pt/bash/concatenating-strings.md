---
title:    "Bash: Concatenando strings"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que concatenar strings no Bash?

O Bash é uma linguagem de programação bastante utilizada no desenvolvimento de scripts e automatização de tarefas no Linux. Uma das funções mais básicas e importantes em qualquer linguagem de programação é a capacidade de manipular strings. E uma das maneiras de fazer isso no Bash é através da concatenação de strings.

Quando trabalhamos com programação, muitas vezes precisamos juntar duas ou mais strings para formar uma nova, seja para apresentar uma informação ao usuário ou para realizar alguma operação específica. Através da concatenação de strings, podemos unir várias partes de texto em uma única string, facilitando assim o nosso trabalho de programação.

## Como fazer a concatenação de strings no Bash?

Para realizar a concatenação de strings no Bash, utilizamos o operador de concatenação de strings `+` entre as duas strings que queremos juntar. Veja abaixo um exemplo de código que demonstra como fazer isso:

```Bash
string1="Olá"
string2="Mundo!"
concatenado="$string1 $string2"
echo $concatenado
```

Output:

```
Olá Mundo!
```

Neste exemplo, declaramos duas variáveis, `string1` e `string2`, que contém as strings que desejamos unir. Em seguida, utilizamos o operador `+` entre elas e atribuímos o resultado a uma nova variável chamada `concatenado`. Por fim, utilizamos o comando `echo` para imprimir o valor da variável `concatenado` na tela.

Além do operador `+`, também é possível fazer a concatenação de strings utilizando o operador `+=`, como mostrado no exemplo abaixo:

```Bash
string1="O"
string2="Bash"
string1+=" "
string1+="$string2"
echo $string1
```

Output:
```
O Bash
```

## Mergulho profundo na concatenação de strings

Além de unir duas ou mais strings, também é possível usar a concatenação para formatar strings e inserir variáveis dentro delas. Veja um exemplo:

```Bash
nome="Fulano"
sobrenome="da Silva"
mensagem="Olá, meu nome é $nome $sobrenome."
echo $mensagem
```

Output:

```
Olá, meu nome é Fulano da Silva.
```

Neste caso, a variável `mensagem` recebe as strings "Olá, meu nome é" e as variáveis `nome` e `sobrenome` juntas através do operador `+`. Dessa forma, podemos criar mensagens personalizadas e dinâmicas de acordo com as variáveis que possuímos.

Além disso, é possível utilizar caracteres especiais, como `\n` para quebrar linhas, e `\t` para tabulação, dentro das strings concatenadas.

## Veja também

- Documentação oficial do Bash: https://www.gnu.org/software/bash/manual/bash.html
- Tutorial de Bash para iniciantes: https://linuxconfig.org/bash-scripting-tutorial-for-beginners
- Lista de comandos e operadores do Bash: https://opensource.com/downloads/bash-cheat-sheet