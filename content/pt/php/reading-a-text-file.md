---
title:    "PHP: Lendo um arquivo de texto"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler arquivos de texto em PHP?

Ler arquivos de texto em PHP é uma habilidade fundamental para qualquer programador que deseja manipular dados armazenados em arquivos. Isso permite que você acesse informações importantes e as utilize em seus códigos.

## Como fazer?

Ler arquivos de texto em PHP é bastante simples. Você pode usar a função `fopen()` para abrir o arquivo e `fgets()` para ler cada linha do arquivo até chegar ao final. Em seguida, você pode usar a função `echo` para imprimir as informações na tela do usuário.

```
<?php
$handle = fopen("arquivo.txt", "r"); // abre o arquivo para leitura
if ($handle) {
  while (($line = fgets($handle)) !== false) {
    echo $line; // imprime cada linha do arquivo
  }
  fclose($handle); // fecha o arquivo
} else {
  echo "Não foi possível abrir o arquivo.";
}
?>
```

O código acima abre o arquivo de texto "arquivo.txt" e lê cada linha até o final, imprimindo-as na tela. Você pode adaptar esse código para realizar outras tarefas, como armazenar o conteúdo do arquivo em uma variável ou manipular os dados de alguma forma.

## Aprofundando-se

Além das funções `fopen()` e `fgets()`, existem outras funções úteis para leitura de arquivos de texto em PHP, como `fgetcsv()` para ler arquivos CSV e `file_get_contents()` para ler um arquivo inteiro de uma só vez. Você também pode usar parâmetros adicionais em `fopen()` para definir o modo de leitura do arquivo, como apenas leitura, leitura e escrita, ou escrita apenas.

Ler arquivos de texto em PHP também pode ser útil para a criação de sistemas de login e registro, onde os dados do usuário são armazenados em um arquivo de texto e, em seguida, são lidos para autenticação.

## Veja também

- [Documentação oficial do PHP para leitura de arquivos de texto](https://www.php.net/manual/en/function.fgets.php)
- [Tutorial sobre leitura de arquivos de texto em PHP](https://www.w3schools.com/php/func_filesystem_fgets.asp)
- [Diferenças entre `fopen()` e `file_get_contents()` para leitura de arquivos de texto](https://stackoverflow.com/questions/15933070/what-is-the-difference-between-file-get-contents-and-fopen-in-php)