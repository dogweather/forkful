---
title:                "PHP: Encontrando o comprimento de uma string"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Porque

Você já se perguntou como os programadores conseguem contar o número de caracteres em uma frase ou palavra? Em PHP, isso é feito através de uma função simples chamada `strlen()`. Neste artigo, vamos explorar por que e como essa função é usada.

## Como Fazer

Primeiro, vamos ver a estrutura básica da função `strlen()`:

```PHP
<?php
$string = "Olá mundo!";
echo strlen($string);
?>
```

Este código irá imprimir o número de caracteres na string, que nesse caso é 11. Isso acontece porque cada caractere, incluindo espaços, é contado pela função `strlen()`. Mas e se uma string tiver caracteres especiais, como acentos ou emojis? A função ainda irá contar corretamente, pois ela não leva em consideração a linguagem ou tipo de caractere.

Agora, vamos ver um exemplo mais prático. Se quisermos limitar o número máximo de caracteres em uma frase, podemos usar a função `strlen()` para verificar esse limite. Veja o exemplo abaixo:

```PHP
<?php
$frase = "A melhor forma de aprender é praticando.";
if(strlen($frase) > 20){
    echo "A sua frase possui mais de 20 caracteres.";
} else {
    echo "A sua frase possui 20 caracteres ou menos.";
}
?>
```

Neste caso, a condição `if` irá verificar se a string possui mais de 20 caracteres e, se sim, irá imprimir uma mensagem de aviso. Você pode alterar esse valor para se adequar às suas necessidades.

## Mergulho Profundo

Agora que já sabemos como usar a função `strlen()` em diferentes situações, vamos entender um pouco mais sobre como ela funciona. Internamente, a função usa um loop para percorrer a string e contar cada caractere individualmente. Isso significa que quanto maior a string, mais tempo a função irá levar para executar.

Além disso, a função `strlen()` só recebe um argumento, que deve ser uma string. Isso significa que se você tentar passar um número ou um array, a função irá retornar um erro. Portanto, sempre certifique-se de usar a função corretamente, passando apenas uma string válida como argumento.

## Veja Também

- [Documentação oficial da função `strlen()` em PHP](https://www.php.net/manual/pt_BR/function.strlen.php)
- [Como usar a função `substr()` em PHP](https://linkparasite.com.br/php/como-usar-a-funcao-substr-em-php/)