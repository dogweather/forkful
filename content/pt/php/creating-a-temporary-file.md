---
title:                "Criando um arquivo temporário"
date:                  2024-01-20T17:40:45.610333-07:00
model:                 gpt-4-1106-preview
simple_title:         "Criando um arquivo temporário"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Criar um arquivo temporário em PHP é como rabiscar em um papel de rascunho. Programadores fazem isso quando querem guardar dados rapidamente sem precisar dar um nome ou um local permanente ao arquivo - é algo feito no calor do momento, que pode desaparecer sem deixar rastros quando não for mais necessário.

## Como Fazer:

```PHP
<?php
// Criando um arquivo temporário
$temp_file = tmpfile();

// Escrevendo dados no arquivo temporário
fwrite($temp_file, "Olá, estou guardado temporariamente!");

// Lendo o que foi escrito
rewind($temp_file);
echo fread($temp_file, 1024);

// O arquivo é removido quando fechamos
fclose($temp_file);
?>
```
Saída de exemplo:
```
Olá, estou guardado temporariamente!
```

## Mergulho Profundo:

Arquivos temporários no PHP são como notas adesivas na programação. Desde os primórdios, quando discos tinham espaço limitado, era essencial poder trabalhar com dados sem ter que criar um arquivo real no sistema. Com `tmpfile()`, o PHP lida com isso para você, criando um arquivo temporário no sistema de arquivos do servidor que é excluído automaticamente quando o script termina ou quando o arquivo é fechado com `fclose()`.

Uma alternativa é `tempnam()`, que cria um nome de arquivo temporário, em vez de um arquivo. Isso dá maior controle, permitindo definir onde o arquivo será criado e garantir sua exclusão manual.

Detalhes de implementação incluem o manuseio correto de permissões e possíveis problemas de segurança. Arquivos temporários devem ser bem geridos para evitar que ocupem espaço desnecessário ou causem conflitos de acesso.

## Veja Também:

- [Documentação oficial do PHP para tmpfile()](https://www.php.net/manual/pt_BR/function.tmpfile.php)
- [Documentação oficial do PHP para tempnam()](https://www.php.net/manual/pt_BR/function.tempnam.php)
- [Guia sobre manuseio de arquivos em PHP](https://www.php.net/manual/pt_BR/ref.filesystem.php)