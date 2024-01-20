---
title:                "Lendo um arquivo de texto"
html_title:           "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Ler um arquivo de texto é um processo de acessar e interpretar o conteúdo de um arquivo no formato de texto. Programadores costumam fazê-lo para extrair, utilizar ou manipular as informações armazenadas nesses arquivos.

## Como Fazer:
Para ler um arquivo de texto em PHP, você pode usar a função file_get_contents() ou a função fopen() combinado com fgets(). Veja abaixo:

```php
//Usando file_get_contents()
$texto = file_get_contents('arquivo.txt');
echo $texto;

//Usando fopen() e fgets()
$arquivo = fopen('arquivo.txt', 'r');
while(!feof($arquivo)) {
   $linha = fgets($arquivo);
   echo $linha;
}
fclose($arquivo);
```
A saída seria o conteúdo de 'arquivo.txt'.

## Mergulho Profundo
Historicamente, a necessidade de ler arquivos de texto surgiu com a evolução dos sistemas operacionais e o aumento da complexidade dos programas. No PHP, as funções file_get_contents() e fopen() são os meios mais comuns de realizar essa tarefa.

Uma alternativa a essas funções seria usar a função file(), que lê o arquivo de texto completo e retorna as linhas em um array. Porém, dependendo do tamanho do arquivo, pode-se preferir o fopen() para controlar o uso de memória.

Quando se usa fopen(), é importante lembrar de fechar o arquivo após a leitura com fclose(). Ignorar esse passo pode resultar em vazamentos de memória.

## Ver Também
Para mais informações sobre a leitura de arquivos de texto em PHP, consulte:
1. A documentação oficial do PHP: [file_get_contents()](https://www.php.net/manual/pt_BR/function.file-get-contents.php), [fopen()](https://www.php.net/manual/pt_BR/function.fopen.php), [fgets()](https://www.php.net/manual/pt_BR/function.fgets.php)
2. Guia de manipulação de arquivos em PHP: [w3schools.com](https://www.w3schools.com/php/php_file.asp)
3. Tutorial sobre leitura de arquivos em PHP: [tutorialspoint.com](https://www.tutorialspoint.com/php/php_files.htm)