---
title:    "PHP: Lendo um arquivo de texto."
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto em PHP?

Ler arquivos de texto é uma tarefa comum em muitos projetos de programação em PHP. Isso pode ser necessário para obter dados de um arquivo externo, como um arquivo de configuração, ou para processar informações de um arquivo gerado dinamicamente. Neste artigo, vamos explorar como podemos ler arquivos de texto em PHP de forma eficiente e eficaz.

## Como Fazer

Para ler um arquivo de texto em PHP, podemos usar a função `file()` que retorna um array contendo cada linha do arquivo de texto como um elemento. Vamos dar uma olhada em um exemplo de código:

```
<?php
// Abre o arquivo de texto
$file = fopen("exemplo.txt", "r");

// Lê o arquivo
$lines = file("exemplo.txt");

// Imprime o array com o conteúdo do arquivo
print_r($lines);

// Fecha o arquivo
fclose($file);
```

Neste exemplo, usamos a função `fopen()` para abrir o arquivo em modo de leitura e em seguida, usamos a função `file()` para ler as linhas do arquivo e armazená-las em um array chamado `$lines`. Por fim, usamos a função `fclose()` para fechar o arquivo.

Podemos também usar a função `fread()` para ler um arquivo de texto em pedaços específicos, em vez de ler todas as linhas de uma vez. Veja o exemplo abaixo:

```
<?php
// Abre o arquivo de texto
$file = fopen("exemplo.txt", "r");

// Lê o arquivo em pedaços de 1024 bytes
$chunk = fread($file, 1024);

// Imprime o conteúdo do pedaço lido
echo $chunk;

// Fecha o arquivo
fclose($file);
```

Com a função `fread()`, podemos controlar o tamanho do pedaço que queremos ler, o que pode ser útil para processar grandes arquivos de texto de forma mais eficiente.

## Aprofundando-se

Para trabalhar com arquivos de texto em PHP, é importante considerar alguns detalhes importantes. Por exemplo, é necessário garantir que o arquivo exista antes de tentar lê-lo. Também é importante lembrar de sempre fechar o arquivo após a leitura, para que os recursos sejam liberados.

Outro detalhe importante é que a função `file()` lê todo o conteúdo do arquivo para a memória, o que pode ser um problema para arquivos muito grandes. Nesses casos, é mais recomendado usar a função `fread()` em pedaços menores, como mostrado no exemplo anterior.

É importante ressaltar também que é possível ler arquivos de texto de outros idiomas, basta especificar a codificação correta ao usar as funções `fopen()` e `fread()`.

## Veja Também

- [Documentação oficial do PHP sobre leitura de arquivos de texto](https://www.php.net/manual/en/function.file.php)
- [Artigo sobre diferenças entre as funções `file()` e `fopen()`](https://www.php.net/manual/en/function.file.php#123084)
- [Exemplo de leitura de arquivo de texto linha por linha](https://www.php.net/manual/en/function.fgets.php#setlocale)