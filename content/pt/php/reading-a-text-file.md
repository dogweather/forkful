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

## Por que ler um arquivo de texto em PHP

Ler arquivos de texto é uma tarefa comum na programação em PHP. Pode ser necessário acessar informações armazenadas em arquivos externos, como configurações ou dados para processamento posterior. Além disso, ler arquivos de texto pode ser útil para importar dados em um banco de dados ou gerar relatórios.

## Como fazer

Ler um arquivo de texto em PHP é simples e pode ser feito em poucas linhas de código. Primeiramente, é necessário abrir o arquivo utilizando a função `fopen()`, que retorna um identificador de arquivo:

```
$handle = fopen('arquivo.txt', 'r');
```

O primeiro parâmetro da função é o nome do arquivo que será lido e o segundo é o modo de leitura, neste caso "r" para leitura. Em seguida, é possível utilizar a função `fgets()` para ler uma linha do arquivo:

```
$linha = fgets($handle);
```

A função `fgets()` retorna uma string contendo a linha lida. É possível percorrer o arquivo utilizando um loop `while` até o final do arquivo ser alcançado:

```
while(!feof($handle)){
    $linha = fgets($handle);
    //processar a linha
}
```

Após a leitura, é importante fechar o arquivo utilizando a função `fclose()`:

```
fclose($handle);
```

## Mais detalhes sobre a leitura de arquivos de texto

Além da função `fgets()`, existem outras opções para ler arquivos de texto em PHP. A função `file_get_contents()` pode ser utilizada para ler todo o conteúdo do arquivo de uma vez, retornando uma string com todos os dados. Já a função `fread()` permite especificar o tamanho máximo de dados a serem lidos, o que pode ser útil para arquivos maiores.

Além disso, é importante lembrar que é possível definir o modo de abertura do arquivo para escrita e leitura utilizando as opções "w" e "w+", respectivamente. Isso permite modificar o conteúdo do arquivo ou criar um novo arquivo, se ele não existir.

## Veja também
- Documentação oficial do PHP sobre leitura de arquivos: http://php.net/manual/pt_BR/function.fopen.php
- Tutorial da W3Schools sobre leitura de arquivos em PHP: https://www.w3schools.com/php/php_file_open.asp