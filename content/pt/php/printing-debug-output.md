---
title:                "PHP: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saída de depuração?

A impressão de saída de depuração é uma técnica comumente usada por programadores para encontrar e solucionar erros em seus códigos. Isso permite que eles vejam o valor de diferentes variáveis e detectem possíveis problemas à medida que o código é executado. Imprimir a saída de depuração também é útil para entender como o código está funcionando e pode ser usado para fins de otimização.

## Como fazer

Para imprimir a saída de depuração em PHP, você pode usar a função "print_r()" ou "var_dump()". Ambas as funções são úteis para visualizar arrays e objetos complexos. Por exemplo:

```PHP
$arr = array(1, 2, 3);

print_r($arr);
// Saída: Array ( [0] => 1 [1] => 2 [2] => 3 )

$object = new StdClass();
$object->name = "João";
$object->age = 25;

var_dump($object);
/* Saída: object(stdClass)#1 (2) {
  ["name"] => string(4) "João"
  ["age"] => int(25)
} */
```

Além disso, você também pode usar a função "echo" para imprimir valores simples, como strings e números. Por exemplo:

```PHP
$name = "Maria";
$age = 30;

echo "Nome: " . $name . ", Idade: " . $age;
// Saída: Nome: Maria, Idade: 30
```

## Mergulho profundo

Além das funções mencionadas acima, existem outras técnicas que podem ser usadas para imprimir saída de depuração em PHP. Alguns programadores preferem usar a diretiva "ini_set('display_errors', 1)" para exibir erros e avisos no navegador. Outros usam a função "error_log()" para imprimir mensagens de erro em um arquivo de log. Você também pode criar suas próprias funções de depuração, personalizadas para suas necessidades específicas.

Uma boa prática é usar a saída de depuração durante o desenvolvimento e removê-la antes de publicar o código em produção, para evitar possíveis problemas de segurança e melhorar a performance do código.

## Veja também

- [Funções de depuração em PHP](https://www.php.net/manual/en/book.debugger.php)
- [Tutorial de depuração em PHP](https://www.php.net/manual/en/debugger-quick-start.php)
- [Artigo sobre depuração de código em PHP](https://www.w3resource.com/php/debugging/diagnosing-php-code.php)