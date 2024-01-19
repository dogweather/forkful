---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Criar um arquivo temporário é um procedimento que consiste na criação de um arquivo com um uso de curto prazo, muitas vezes usado para armazenar dados temporariamente durante a execução de um código. Programadores fazem isso quando precisam de um armazenamento temporário que é mais rápido e fácil de acessar do que um banco de dados, por exemplo.

## Como Fazer:

Aqui temos um exemplo de como criar um arquivo temporário em PHP:

```PHP
<?php
$tmpfile = tmpfile();
fwrite($tmpfile, "Olá Mundo!");

rewind($tmpfile);

echo fread($tmpfile, 1024); // Isto irá imprimir "Olá Mundo!"
?>
```
Neste exemplo, a função `tmpfile()` retorna um recurso de arquivo, e, ao mesmo tempo, cria um arquivo temporário. O `fwrite()` é então usado para escrever a string "Olá Mundo!" no arquivo temporário. O `rewind()` é usado para reposicionar o ponteiro do arquivo no início, permitindo que o `fread()` leia e imprima a string que acabamos de escrever.

## Aprofundamento

Historicamente, a criação de arquivos temporários tem sido usada em programação desde os primeiros dias de computação. É uma maneira rápida e fácil de evitar o uso de memória ao lidar com grandes volumes de dados.

Como alternativa à função `tmpfile()`, podemos usar a função `tempnam()` do PHP. Esta função cria um nome de arquivo único, que pode então ser usado para criar um arquivo usando funções como `fopen()`.

No que diz respeito à implementação, quando a função `tmpfile()` é chamada, o PHP cria um arquivo temporário com um nome único em um diretório temporário. Este arquivo é automaticamente removido quando o script PHP termina.

## Veja Também

1. [Função tmpfile() - Manual do PHP](https://www.php.net/manual/pt_BR/function.tmpfile.php)

2. [Função tempnam() - Manual do PHP](https://www.php.net/manual/pt_BR/function.tempnam.php)

3. [Função fopen() - Manual do PHP](https://www.php.net/manual/pt_BR/function.fopen.php)