---
title:                "Escrevendo para o erro padrão"
html_title:           "PHP: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que

Alguma vez você já recebeu uma mensagem de erro ao executar um script PHP e se perguntou como ela foi gerada? Bem, a resposta está em escrever para o erro padrão (standard error). Neste artigo, vamos explorar por que isso pode ser útil e como fazer isso de forma simples em seus próprios códigos PHP.

## Como Fazer

Escrever para o erro padrão pode ser especialmente útil em situações onde você precisa depurar seu código e imprimir valores de variáveis ou mensagens de erro. Para isso, você pode usar a função `fwrite(STDERR, $variavel)` do PHP, que envia o conteúdo da variável para o erro padrão.

Por exemplo, se tivermos o seguinte código PHP que está gerando um erro:

```PHP
<?php
$nome = "João";
if($nome == "Maria")
    echo "Olá, $nome!";
else
    echo "Desculpe, ocorreu um erro!";
```

Podemos adicionar `fwrite(STDERR, $nome);` após a condição `if` para enviar o valor da variável `$nome` para o erro padrão:

```PHP
<?php
$nome = "João";
if($nome == "Maria")
    echo "Olá, $nome!";
else{
    fwrite(STDERR, $nome);
    echo "Desculpe, ocorreu um erro!";
}
```

Isso nos dará o seguinte resultado no console:

```
JoãoDesculpe, ocorreu um erro!
```

Você pode utilizar a função `fwrite()` em qualquer lugar no seu código onde precise enviar uma mensagem para o erro padrão. É uma forma simples de depurar seu código e identificar problemas com variáveis e condições.

## Deep Dive

Agora que você já sabe como escrever para o erro padrão, vamos nos aprofundar um pouco mais em como essa função funciona. Primeiramente, é importante entender que o erro padrão é como um canal de comunicação entre o PHP e o terminal, que é onde as mensagens de erro são exibidas para o usuário.

Quando você executa um script PHP no terminal e ele encontra um erro, ele é enviado diretamente para o erro padrão e é exibido no seu console. No entanto, se você quiser exibir alguma informação específica, pode usar a função `fwrite()` para enviar essa mensagem diretamente para o erro padrão.

Além disso, é importante lembrar que o erro padrão é diferente do erro de saída padrão (standard output). O erro de saída padrão é onde as mensagens de saída do seu script são exibidas, enquanto o erro padrão é reservado especificamente para mensagens de erro.

## Veja também

- [Documentação oficial do PHP para a função fwrite](https://www.php.net/manual/en/function.fwrite.php)
- [Artigo sobre como depurar códigos PHP](https://www.php.net/manual/en/function.fwrite.php)