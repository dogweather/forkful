---
title:                "PHP: Criando um arquivo temporário"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em PHP?

Criar um arquivo temporário pode ser útil em várias situações de programação. Isso pode ser necessário quando se trabalha com arquivos grandes, ou quando se precisa armazenar dados temporários antes de manipulá-los. Além disso, é uma boa prática em termos de segurança, pois o arquivo é excluído automaticamente após o uso.

## Como criar um arquivo temporário em PHP

Para criar um arquivo temporário em PHP, usamos a função `tmpfile ()`. Esta função cria um arquivo único e vazio no diretório temporário do sistema operacional. Você pode manipular esse arquivo da mesma forma como faria com qualquer outro arquivo.
Vamos dar uma olhada em um exemplo simples de como usar a função `tmpfile ()`:

```PHP
<?php
$file = tmpfile(); // cria um arquivo temporário

// Escreve dados no arquivo temporário
fwrite($file, "Este é um arquivo temporário criado em PHP.");

// Lê o conteúdo do arquivo
rewind($file);
echo fread($file, filesize($file));

// Fecha e exclui o arquivo
fclose($file);
?>
```

O código acima criará um arquivo temporário e escreverá uma string nele. Em seguida, ele lerá o conteúdo do arquivo e o excluirá. Você pode verificar o diretório temporário do sistema operacional para ver o arquivo sendo criado e excluído automaticamente.

## Explorando mais a fundo a criação de arquivos temporários

A função `tmpfile ()` possui algumas opções adicionais que permitem personalizar a criação do arquivo temporário. Além disso, você pode usar várias outras funções PHP, como `tmpnam ()` e `tempnam ()`, para criar arquivos temporários com mais controle sobre o nome e a localização do arquivo.

Veja a documentação doPHP para obter mais informações e exemplos sobre a criação de arquivos temporários em PHP.

## Ver também

Aqui estão algumas úteis fontes para ler mais sobre a criação de arquivos temporários em PHP:

- [Documentação do PHP sobre criação de arquivos temporários](https://www.php.net/manual/en/function.tmpfile.php)
- [Blog sobre boas práticas de segurança ao criar arquivos temporários em PHP](https://www.owasp.org/index.php/Unrestricted_File_Upload)
- [Discussão sobre o uso de arquivos temporários em sistemas Windows e Linux](https://stackoverflow.com/questions/941012/what-is-the-difference-between-appdata-and-programdata)