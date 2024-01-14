---
title:    "PHP: Lendo argumentos da linha de comando"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando?

Ler argumentos da linha de comando é uma habilidade essencial para programadores PHP. Com essa habilidade, é possível criar scripts mais dinâmicos e interativos, permitindo que seus programas recebam dados do usuário em tempo de execução. Neste artigo, exploraremos como ler argumentos da linha de comando em PHP e como isso pode melhorar seus projetos.

## Como Ler Argumentos da Linha de Comando em PHP

Ler argumentos da linha de comando em PHP é surpreendentemente simples. Tudo o que você precisa é da função `$_SERVER["argv"]`, que retorna um array contendo todos os argumentos fornecidos na linha de comando. Vamos dar uma olhada em um exemplo básico:

```PHP
<?php

// Lê o primeiro argumento fornecido na linha de comando
$nome = $_SERVER["argv"][1];

echo "Olá, " . $nome . "!";

// $ php cumprimentar.php João
// Saída: Olá, João!
```

No exemplo acima, estamos usando o segundo elemento do array `$_SERVER["argv"]`, pois o primeiro é sempre o nome do script em si. Agora, vamos ver como podemos usar isso para criar um programa que calcula a média de vários números fornecidos pelo usuário:

```PHP
<?php

// Remove o nome do script do array $_SERVER["argv"]
$argumentos = array_slice($_SERVER["argv"], 1);

// Converte todos os argumentos em números e calcula a média
$media = array_sum($argumentos) / count($argumentos);

echo "A média é: " . $media;

// $ php media.php 5 10 15 20
// Saída: A média é: 12.5
```

É importante notar que os argumentos fornecidos na linha de comando são sempre tratados como strings em PHP, portanto, é necessário convertê-los para o tipo de dados apropriado, como fizemos no exemplo acima.

## Aprofundando nos Argumentos da Linha de Comando

Agora que sabemos como ler argumentos da linha de comando e usá-los em nossos programas, é importante entender como isso funciona por trás dos bastidores. Quando você executa um script PHP na linha de comando, é criado um ambiente separado chamado "CLI", que é responsável por receber e processar esses argumentos. É por isso que podemos usar a função `$_SERVER["argv"]`, que é exclusiva para scripts executados na linha de comando.

Além disso, é importante notar que os argumentos fornecidos na linha de comando são sempre separados por espaços em branco. Se você quiser usar argumentos que contenham espaços em branco, é necessário encapsulá-los entre aspas.

## Veja Também

Aqui estão alguns recursos adicionais para aprender mais sobre como ler argumentos da linha de comando em PHP:

- [Documentação Oficial do PHP sobre argumentos da linha de comando](https://www.php.net/manual/pt_BR/features.commandline.arguments.php)
- [Vídeo tutorial do canal PHP Academy](https://www.youtube.com/watch?v=M1WvfFdcVMk)
- [Artigo do site PHPJabbers](https://www.phpjabbers.com/capture-command-line-arguments-in-a-php-script-php41)