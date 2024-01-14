---
title:                "PHP: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em PHP?

Criar um arquivo temporário em PHP pode ser útil em diversas situações, como por exemplo, quando é necessário manipular dados sensíveis ou quando se trabalha com uploads de imagens ou documentos temporários. Além disso, criar um arquivo temporário pode ajudar a economizar espaço em disco ao realizar operações temporárias.

## Como criar um arquivo temporário em PHP

Para criar um arquivo temporário em PHP, utilizamos a função `tempnam()`. Essa função recebe dois parâmetros, o primeiro é o caminho onde o arquivo temporário será armazenado e o segundo é o prefixo do nome do arquivo. Em seguida, podemos escrever dados no arquivo temporário utilizando a função `fwrite()` e ler esses dados com a função `fread()`.

```PHP
$tempFile = tempnam('/caminho/do/arquivo', 'prefixo');
$fileHandle = fopen($tempFile, 'a+');
fwrite($fileHandle, 'Dados a serem escritos no arquivo');
```

Para visualizar o conteúdo do arquivo temporário, podemos utilizar a função `file_get_contents()`.

```PHP
echo file_get_contents($tempFile);
```

A saída será "Dados a serem escritos no arquivo".

## Mergulhando mais fundo na criação de arquivos temporários

Quando criamos um arquivo temporário em PHP, ele é armazenado em uma pasta temporária do sistema operacional. Podemos descobrir o caminho dessa pasta através da função `sys_get_temp_dir()`. Além disso, podemos especificar o sufixo do nome do arquivo como terceiro parâmetro da função `tempnam()`, caso queiramos criar um arquivo com uma extensão específica.

```PHP
$tempFile = tempnam('/caminho/do/arquivo', 'prefixo', '.txt');
```

Como precaução, devemos sempre deletar o arquivo temporário após utilizá-lo, para não deixar dados sensíveis expostos. Isso pode ser facilmente feito com a função `unlink()`.

## Veja também

- [Documentação oficial do PHP sobre a função `tempnam()`](https://www.php.net/manual/pt_BR/function.tempnam.php)
- [Mais informações sobre arquivos temporários em PHP](https://www.php.net/manual/pt_BR/reserved.variables.php)