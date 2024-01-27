---
title:                "Escrevendo um arquivo de texto"
date:                  2024-01-19
html_title:           "Arduino: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?

Escrever um arquivo de texto em PHP é o processo de salvar dados em um arquivo no sistema de arquivos. Programadores fazem isso para persistir informação, configurar sistemas, registrar logs ou exportar dados.

## Como Fazer:

Exemplo simples para criar e escrever em um arquivo de texto:

```php
<?php
$texto = "Olá, mundo! Escrevendo em um arquivo.\n";
$file = 'meu_arquivo.txt';

// Abre o arquivo ou cria se não existir, e então escreve o texto
file_put_contents($file, $texto);

// Para acrescentar ao arquivo existente, use o flag FILE_APPEND
file_put_contents($file, $texto, FILE_APPEND);
?>
```

Saída esperada: Se você verificar o conteúdo de `meu_arquivo.txt`, verá o texto escrito nele.

## Aprofundamento:

Escrever em arquivos de texto é uma técnica conhecida desde os primórdios da computação. Alternativas incluem bancos de dados ou armazenamento em nuvem, mas arquivos de texto permanecem populares pela simplicidade e portabilidade. Ao implementar, é fundamental lidar com permissões do sistema de arquivos e possíveis erros, usando abordagens como `file_put_contents()` ou, para mais controle, `fopen()`, `fwrite()` e `fclose()`.

## Veja Também:

- [Documentação oficial do PHP sobre manipulação de arquivos](https://www.php.net/manual/pt_BR/book.filesystem.php)
- [Tutorial W3Schools sobre sistema de arquivos em PHP](https://www.w3schools.com/php/php_file.asp)
- [Artigo sobre boas práticas para manipulação de arquivos com PHP](https://www.phptherightway.com/#files)
