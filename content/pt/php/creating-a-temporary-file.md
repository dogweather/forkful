---
title:                "Criando um arquivo temporário"
html_title:           "PHP: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em PHP?

Há diversas razões pelas quais alguém pode precisar criar um arquivo temporário em PHP. Ele pode ser útil para armazenar informações temporárias, como logs ou caches, que precisam ser acessadas de forma rápida, mas não precisam ser permanentes. Além disso, esse recurso também pode ser utilizado para gerar downloads de arquivos que precisam ser temporários, sem ocupar espaço no servidor.

## Como criar um arquivo temporário em PHP

Para criar um arquivo temporário em PHP, utilize a função `tempnam()`. Ela recebe dois parâmetros: o diretório onde o arquivo será criado e o prefixo do nome do arquivo. Veja um exemplo abaixo:

```PHP
$tempFile = tempnam('/var/www/temp/', 'php_temp');
```

Esse código irá criar um arquivo temporário no diretório "/var/www/temp/" com o prefixo "php_temp". Você pode então utilizar esse arquivo para armazenar suas informações temporárias.

Você também pode definir um tempo de vida para o arquivo temporário utilizando a função `tempnam()`. Por padrão, o arquivo será excluído após o script ser executado, mas você pode especificar um tempo em segundos como terceiro parâmetro para definir um tempo de vida diferente. Veja o exemplo abaixo:

```PHP
$tempFile = tempnam('/var/www/temp/', 'php_temp', 300); // arquivo será excluído após 5 minutos (300 segundos)
```

## Mergulho Profundo: Mais informações sobre a criação de arquivos temporários em PHP

Além da função `tempnam()`, existem outras formas de criar arquivos temporários em PHP, como a função `tmpfile()` que retorna um ponteiro para o arquivo temporário criado.

Também é importante lembrar que arquivos temporários devem ser utilizados com cautela, pois podem ocupar espaço desnecessário no servidor e prejudicar o desempenho. Portanto, sempre certifique-se de excluí-los quando não forem mais necessários.

## Veja também

- [Documentação oficial do PHP para função `tempnam()`](https://www.php.net/manual/pt_BR/function.tempnam.php)
- [Documentação oficial do PHP para função `tmpfile()`](https://www.php.net/manual/pt_BR/function.tmpfile.php)
- [Como usar arquivos temporários em PHP](https://www.php.net/manual/pt_BR/tutorial.filesystem.tmpfile.php)