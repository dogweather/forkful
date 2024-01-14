---
title:    "PHP: Imprimindo saída de depuração"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/php/printing-debug-output.md"
---

{{< edit_this_page >}}

# Por que imprimir saída de debug?

Imprimir saída de debug é uma prática comum em programação PHP. Isso permite que os desenvolvedores examinem o funcionamento interno de seu código, encontrando e corrigindo possíveis erros e problemas. Além disso, ao imprimir informações de depuração, os desenvolvedores podem entender melhor como seu código está funcionando e tomar decisões mais informadas sobre possíveis melhorias.

## Como fazer

Para imprimir saída de debug em PHP, você pode usar a função `print_r()` ou `var_dump()`. Ambas as funções exibirão informações sobre uma ou mais variáveis ​​na tela. Veja um exemplo abaixo:

```PHP
<?php
$nome = "João";
print_r($nome);
// Saída: João
var_dump($nome);
// Saída: string(4) "João"
?>
```

Além disso, também é possível usar a função `echo` para imprimir informações de debug. No entanto, é importante ter em mente que a função `echo` só pode imprimir uma string simples, enquanto `print_r()` e `var_dump()` podem imprimir qualquer tipo de dados, incluindo arrays e objetos.

## Mergulho profundo

Existem várias opções e parâmetros que podem ser usados ​​para personalizar a saída de debug em PHP. Por exemplo, você pode usar o parâmetro `die` com `print_r()` ou `var_dump()` para interromper a execução do código e ver a saída em um ponto específico do seu código. Além disso, você também pode formatar a saída de acordo com suas necessidades, definindo parâmetros adicionais nas funções.

Outra ferramenta útil para imprimir saída de debug é o Xdebug, um depurador para PHP que permite inspecionar valores de variáveis, rastrear a execução do código e encontrar problemas de desempenho. Para usá-lo, você precisará instalá-lo e configurá-lo corretamente em seu ambiente de desenvolvimento.

# Veja também

- [Documentação do PHP sobre print_r()](https://www.php.net/manual/pt_BR/function.print-r.php)
- [Documentação do PHP sobre var_dump()](https://www.php.net/manual/pt_BR/function.var-dump.php)
- [Documentação do PHP sobre echo](https://www.php.net/manual/pt_BR/function.echo.php)
- [Site oficial do Xdebug](https://xdebug.org/)