---
title:                "PHP: Lendo argumentos da linha de comando"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando

Entender como ler argumentos da linha de comando pode ser extremamente útil para programadores PHP. Isso permite que você crie scripts mais dinâmicos e interativos, além de facilitar a comunicação com outros programas e ferramentas.

## Como fazer

Para ler argumentos da linha de comando em PHP, utilizamos a função `getopt()` que recebe três parâmetros: uma string com as opções desejadas, um array com as opções longas e outro com as opções curtas. Por exemplo:

```PHP
//Variáveis para armazenar os argumentos
$nome = '';
$idade = '';

//Definindo as opções a serem lidas
$opcoes = "n:i:";

//Definindo as opções longas
$opcoes_longas = array(
  "nome:",
  "idade:"
);

//Armazenando os argumentos na variável $resultado
$resultado = getopt($opcoes, $opcoes_longas);

//Atribuindo os valores aos argumentos
if (isset($resultado['n'])) {
  $nome = $resultado['n'];
}

if (isset($resultado['i'])) {
  $idade = $resultado['i'];
}

//Imprimindo os resultados
echo "O nome é: " . $nome . PHP_EOL;
echo "A idade é: " . $idade . PHP_EOL;
```

Ao executar o script com os argumentos `--nome João --idade 25`, o output será:

```
O nome é: João
A idade é: 25
```

## Mergulho profundo

Além da função `getopt()`, também é possível utilizar a variável global `$argv` para acessar os argumentos da linha de comando. Ela armazena todos os argumentos como um array, sendo que o primeiro elemento é o nome do script em execução. Por exemplo:

```PHP
//Armazenando os argumentos na variável $argumentos
$argumentos = $argv;

//Removendo o primeiro elemento do array (nome do script)
array_shift($argumentos);

//Imprimindo os resultados
foreach ($argumentos as $argumento) {
  echo $argumento . PHP_EOL;
}
```

Ao executar o script com os argumentos `João 25`, o output será:

```
João
25
```

## Veja também

- [Documentação da função `getopt()`](https://www.php.net/manual/pt_BR/function.getenv.php)
- [Documentação da variável global `$argv`](https://www.php.net/manual/pt_BR/reserved.variables.argv.php)
- [Tutorial sobre argumentos da linha de comando em PHP](https://www.php.net/manual/pt_BR/features.commandline.php)