---
title:                "Lendo um arquivo de texto"
html_title:           "PHP: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O que e porquê?

Ler um arquivo de texto é uma tarefa comum para programadores em PHP. Isso envolve abrir um arquivo de texto, ler seu conteúdo e armazená-lo em uma variável para uso posterior. Os programadores geralmente fazem isso para processar grandes quantidades de dados ou para realizar operações em dados armazenados em um arquivo.

## Como fazer:

Para ler um arquivo de texto em PHP, podemos usar a função ```file_get_contents()```. Por exemplo, se tivermos um arquivo chamado "dados.txt" com o seguinte conteúdo:

```
Nome: João
Idade: 25
Profissão: Programador
```
Podemos usar o seguinte código para ler o arquivo e exibir seu conteúdo na tela:

```
$arquivo = file_get_contents('dados.txt');
echo $arquivo;
```
A saída seria:

```
Nome: João
Idade: 25
Profissão: Programador
```

## Deep Dive:

Ler arquivos de texto tem sido uma parte fundamental da programação desde os primeiros dias da linguagem. Antes do advento do PHP, os programadores precisavam usar outras linguagens, como Perl ou C, para ler e processar arquivos de texto. No entanto, com a introdução da função ```file_get_contents()```, a leitura de arquivos de texto se tornou mais fácil e conveniente em PHP.

Há também outras opções para ler arquivos de texto em PHP, como o uso da função ```fopen()``` para abrir e ler um arquivo linha por linha ou o uso da extensão pecl-fileinfo para obter informações sobre o tipo de um arquivo antes de lê-lo.

## Ainda mais:

Para saber mais sobre como ler arquivos de texto em PHP, confira a documentação oficial do PHP sobre a função ```file_get_contents()``` (http://php.net/manual/pt_BR/function.file-get-contents.php) e a função ```fopen()``` (http://php.net/manual/pt_BR/function.fopen.php).

Você também pode explorar as possibilidades oferecidas pela extensão pecl-fileinfo (http://php.net/manual/pt_BR/intro.fileinfo.php) para obter informações sobre arquivos antes de lê-los.

Com este conhecimento, você estará pronto para ler e processar arquivos de texto em seus projetos em PHP de maneira eficiente e eficaz.