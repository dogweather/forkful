---
title:                "Lendo argumentos da linha de comando"
html_title:           "PHP: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que

Você pode estar se perguntando: "Por que eu deveria ler sobre argumentos de linha de comando em PHP?".Bem, os argumentos de linha de comando são uma forma útil e eficiente de interagir com um programa em PHP sem ter que acessá-lo por meio de uma interface gráfica. Isso pode ser especialmente útil para scripts automatizados e projetos em desenvolvimento.

## Como Fazer

Para ler argumentos de linha de comando em PHP, vamos usar a função `getopt()`. Esta função é responsável por analisar os argumentos e retornar um array com as opções definidas. Veja abaixo um exemplo simples:

```PHP
<?php
// Definindo as opções
$opts = "hvf:";

// Obtendo as opções e argumentos
$options = getopt($opts);

// Imprimindo a saída
print_r($options);
?>
```

Este código define três opções: `h`, `v` e `f`. A opção `h` é uma opção simples, ou seja, não precisa de um argumento vinculado a ela. A opção `v` é similar, mas será utilizada para imprimir informações adicionais. Por fim, a opção `f` precisa de um argumento, indicando um arquivo para ser manipulado. Agora, vamos verificar a saída quando executamos este script da seguinte forma: `php meu_script.php -h -vf arquivo.txt`.

A saída será um array contendo as opções e os argumentos fornecidos para cada uma delas:

```
Array
(
    [h] => 1
    [v] => 1
    [f] => arquivo.txt
)
```

Como você pode ver, a função `getopt()` automaticamente converte as opções simples em 1 e atribui o argumento fornecido para as opções que precisam de um.

## Mergulho Profundo

A função `getopt()` também oferece a opção de especificar opções de forma mais específica, como indicar se a opção precisa de um argumento ou não, o tipo de argumento esperado e mais. Você pode aprender mais sobre isso na [documentação oficial do PHP](https://www.php.net/manual/pt_BR/function.getopt.php).

Outra forma de ler argumentos de linha de comando em PHP é através da variável global `argv`, que retorna um array com todos os argumentos fornecidos ao script. O primeiro item do array será o nome do script em si, então basta percorrer os itens seguintes para acessar os argumentos. Veja um exemplo abaixo:

```PHP
<?php
// Imprimindo a saída
print_r($argv);
?>
```

Ao executar este script com os mesmos argumentos do exemplo anterior, a saída será a seguinte:

```
Array
(
    [0] => meu_script.php
    [1] => -h
    [2] => -vf
    [3] => arquivo.txt
)
```

Você pode ler mais sobre o uso da variável `argv` [aqui](https://www.php.net/manual/pt_BR/reserved.variables.argv.php).

## Veja Também

Aqui estão alguns links úteis para você aprender mais sobre leitura de argumentos de linha de comando em PHP:

- [Documentação oficial do PHP: getopt()](https://www.php.net/manual/pt_BR/function.getopt.php)
- [Documentação oficial do PHP: Variável global argv](https://www.php.net/manual/pt_BR/reserved.variables.argv.php)
- [Exemplo prático de leitura de argumentos de linha de comando com getopt()](https://www.php.net/manual/pt_BR/function.getopt.php#example-4137)