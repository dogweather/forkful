---
title:    "PHP: Criando um arquivo temporário"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Por que criar arquivos temporários em PHP?

Criar arquivos temporários pode ser útil em diversos casos, como por exemplo, para armazenar dados temporários, gerar arquivos de log ou criar backups em tempo de execução. Além disso, pode ser uma maneira de economizar espaço em disco, já que esses arquivos serão automaticamente excluídos após o uso.

## Como criar arquivos temporários em PHP

Para criar um arquivo temporário em PHP, podemos usar a função `tmpfile()`. Ela retorna um ponteiro para o arquivo temporário criado, que pode ser usado para escrever e ler dados.

```
<?php
$arquivo_temp = tmpfile();
if($arquivo_temp){
    fwrite($arquivo_temp, "Esse é um arquivo temporário criado em PHP!");
    fseek($arquivo_temp, 0); // volta para o início do arquivo
    echo fgetc($arquivo_temp); // imprime o primeiro caractere do arquivo
    fclose($arquivo_temp); // fecha o arquivo temporário
}
```
O código acima irá criar um arquivo temporário, escrever um texto nele, imprimir o primeiro caractere e fechá-lo.

## Aprofundando no assunto

A função `tmpfile()` cria um arquivo temporário de forma segura, garantindo que o nome do arquivo não seja usado por outro processo e que o arquivo seja excluído automaticamente após o uso. No entanto, é possível especificar um nome para o arquivo temporário usando a função `tempnam()`, que aceita dois parâmetros: o diretório onde o arquivo será criado e um prefixo opcional.

```
<?php
$temp_dir = '/tmp/';
$prefix = 'arquivo_temp';
$arquivo_temp = tempnam($temp_dir, $prefix);
```

Além disso, podemos controlar a duração do arquivo temporário usando a função `stream_set_timeout()`. Por exemplo, podemos definir um tempo limite para a exclusão do arquivo, caso ele ainda esteja aberto após o uso.

# Veja também

- [Documentação oficial do PHP sobre arquivos temporários](https://www.php.net/manual/pt_BR/function.tmpfile.php)
- [Tutorial sobre arquivos temporários em PHP](https://www.tutorialspoint.com/php/php_files.htm)