---
title:                "Escrevendo um arquivo de texto"
html_title:           "PHP: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O que e por que?

Escrever um arquivo de texto é simplesmente criar um documento que contém texto legível para máquinas. Os programadores geralmente fazem isso para armazenar dados importantes que podem ser facilmente acessados e manipulados pelos seus códigos.

## Como fazer:

```
<?php
$texto = "Olá mundo!"; // texto que será escrito no arquivo

// abre o arquivo em modo escrita
$arquivo = fopen("arquivo.txt", "w") or die("Não foi possível criar o arquivo!");

// escreve o texto no arquivo
fwrite($arquivo, $texto);

// fecha o arquivo
fclose($arquivo);

// verifica se o texto foi adicionado ao arquivo
echo "O texto foi escrito no arquivo com sucesso!";
?>
```

Saída: O texto foi escrito no arquivo com sucesso!

## Mergulho profundo:

Escrever arquivos de texto é uma prática comum em programação desde os primórdios da linguagem PHP. Existem várias maneiras de fazer isso, como usando funções nativas do PHP como `file_put_contents()` ou bibliotecas externas como o "PHP-Excel". Além disso, é importante lembrar de sempre fechar o arquivo depois de escrever nele para evitar erros e vazamentos de memória.

## Veja também:

- [Função `fopen()` do PHP](https://www.php.net/manual/pt_BR/function.fopen.php)
- [Biblioteca PHP-Excel](https://github.com/PHPOffice/PHPExcel)