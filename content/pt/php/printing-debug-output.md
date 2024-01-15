---
title:                "Imprimindo saída de depuração"
html_title:           "PHP: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que

Você já se deparou com um erro em seu código PHP e não tinha ideia de onde estava o problema? Ou talvez você esteja apenas curioso para ver o que acontece nos bastidores de sua aplicação enquanto ela é executada. Seja qual for o motivo, imprimir saídas de depuração pode ser um recurso valioso para identificar problemas e entender melhor como seu código está funcionando.

## Como Fazer

A impressão de saídas de depuração em PHP é feita através da função nativa `var_dump()`. Esta função aceita um ou mais argumentos e apresenta uma representação visual de seu valor e tipo. Aqui está um exemplo:

```PHP
$nome = "João";
$sobrenome = "Silva";

var_dump($nome, $sobrenome);
```

Este código produzirá a seguinte saída:

```PHP
string(4) "João"
string(5) "Silva"
```

Isso mostra que `nome` é uma string com 4 caracteres e `sobrenome` é uma string com 5 caracteres. Você também pode usar a função `print_r()` para imprimir saídas de depuração, que produzirá uma saída mais legível para humanos. Por exemplo:

```PHP
$numero = 10;
$array = [1, 2, 3];

print_r($numero);
print_r($array);
```

Isso resultará em:

```PHP
10
Array
(
    [0] => 1
    [1] => 2
    [2] => 3
)
```

## Deep Dive

Além de usar as funções `var_dump()` e `print_r()` para imprimir saídas de depuração de forma básica, você também pode personalizar ainda mais a forma como as informações são exibidas. Isso é especialmente útil quando se trabalha com arrays multidimensionais ou objetos. Por exemplo, você pode definir opções para `var_dump()` usando `ini_set()` para controlar a profundidade máxima de exibição de array e a defeinição de recuo para veementes aninhadas. Além disso, você pode usar a função `debug_zval_dump()` para imprimir o uso de memória de cada variável. Experimente com diferentes maneiras de imprimir saídas de depuração e encontre o que funciona melhor para você.

## Veja Também

- [Documentação do PHP - Depuração](https://www.php.net/manual/pt_BR/debugger.php)
- [Debugging in PHP: A Beginner's Guide](https://www.hostinger.com.br/tutoriais/debugging-in-php-guide/) (em inglês)
- [7 dicas para depurar seu código PHP mais rápido e melhor](https://medium.com/@HerbieHommel/7-dicas-para-depurar-seu-c%C3%B3digo-php-mais-r%C3%A1pido-e-melhor-1b06857615c4) (em inglês)