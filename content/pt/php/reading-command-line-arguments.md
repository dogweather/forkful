---
title:                "Lendo argumentos de linha de comando"
html_title:           "Arduino: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O Que e Por Que?

Ler argumentos de linha de comando é uma capacidade que permite a um programa PHP ler e interpretar argumentos ou parâmetros adicionais diretamente da linha de comando quando executado. Programadores fazem isso para incluir detalhes específicos ou opções variáveis no momento da execução do script.

## Como Fazer:

Aqui está um simples exemplo de como podemos ler argumentos de linha de comando em PHP. Execute o código abaixo em uma linha de comando.

```PHP
<?php
// checando se há argumentos de linha de comando
if($argc > 1){
    echo "O primeiro argumento é: " . $argv[1];
} else {
    echo "Nenhum argumento foi fornecido.";
}
?>
```

Depois, experimente o script com e sem um argumento (como `php script.php argumento1`). A saída será, respectivamente:

```bash
O primeiro argumento é: argumento1
```

```bash
Nenhum argumento foi fornecido.
```

## Mergulho Profundo

Historicamente, o suporte para argumentos de linha de comando em PHP foi adicionado no PHP 4.3.0. A global `$argv` contém uma lista de argumentos, enquanto `$argc` contém o número total de argumentos fornecidos.

Existem alternativas para a leitura de argumentos de linha de comando. Por exemplo, a função `parse_str()` permite que argumentos sejam lidos em formato de string de query URL.

A implementação de leitura de argumentos de linha de comando é nativa no PHP, então não há necessidade de instalações ou bibliotecas adicionais.

## Veja Também:

1. Documentação oficial do PHP para Obter Opções de Console: [link](https://www.php.net/manual/en/function.getopt.php)
2. Artigo sobre Argumentos da Linha de Comando PHP na Php.net: [link](https://www.php.net/manual/pt_BR/reserved.variables.argv.php)
3. Artigo do PhpDelusions sobre Argumentos da Linha de Comando: [link](https://phpdelusions.net/articles/cli)