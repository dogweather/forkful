---
title:    "PHP: Encontrando o comprimento de uma string."
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por que

Encontrar o comprimento de uma string é uma tarefa comum na programação e é essencial ter esse conhecimento para manipular e trabalhar com strings em PHP. Saber como encontrar o comprimento de uma string pode ajudar a evitar erros e facilitar o seu trabalho na escrita de códigos.

## Como fazer

Existem duas maneiras de encontrar o comprimento de uma string em PHP: utilizando a função `strlen()` ou utilizando o método `strlen()` da classe `StringClass`.

Um exemplo usando a função `strlen()`:

```PHP
$frase = "Este é um exemplo de string";
echo strlen($frase);
```

A saída será: `29`, que é o número de caracteres na frase.

Outro exemplo usando o método `strlen()` da classe `StringClass`:

```PHP
$frase = "Este é um exemplo de string";
echo $frase->strlen();
```

O resultado será o mesmo: `29`.

## Profundidade

Além da função `strlen()` e do método `strlen()` da classe `StringClass`, você também pode usar a propriedade `length` do objeto string para encontrar o seu comprimento:

```PHP
$frase = "Este é um exemplo de string";
echo $frase->length;
```

Neste caso, a saída também será `29`.

É importante ressaltar que o comprimento de uma string é calculado levando em consideração todos os caracteres, incluindo espaço em branco e caracteres especiais.

## Veja também

- Documentação oficial do PHP: https://www.php.net/manual/pt_BR/function.strlen.php
- Tutorial de strings em PHP: https://www.tutorialspoint.com/php/php_strings.htm
- Método `strlen()` da classe `StringClass`: https://www.php.net/manual/pt_BR/class.stringclass.php