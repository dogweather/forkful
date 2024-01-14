---
title:    "PHP: Juntando cadeias de caracteres"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Porque

A concatenação de strings é uma técnica muito útil na programação PHP para combinar duas ou mais strings em uma única string. Isso pode ser útil para criar mensagens, gerar saídas de dados ou manipular informações em seu código. É uma maneira eficiente de tornar seu código mais dinâmico e flexível.

## Como fazer

Para concatenar strings em PHP, é necessário usar o operador `.` (ponto). Veja um exemplo de como fazer isso abaixo:

```PHP
<?php
$primeiro_nome = "João";
$sobrenome = "Silva";
$nome_completo = $primeiro_nome . " " . $sobrenome;
echo $nome_completo;
```

Neste exemplo, declaramos duas variáveis contendo o primeiro nome e sobrenome de uma pessoa. Em seguida, usamos o operador `.` para concatená-las em uma única string, adicionando também um espaço entre elas. Por fim, usamos o comando `echo` para imprimir o resultado na tela, que seria "João Silva".

Além disso, também é possível usar a função `sprintf()` para concatenar strings. Esta função permite que você substitua marcadores de posição (%s) para as variáveis desejadas. Veja um exemplo:

```PHP
<?php
$primeiro_nome = "Maria";
$sobrenome = "Gomes";
$nome_completo = sprintf("Olá, meu nome é %s %s", $primeiro_nome, $sobrenome);
echo $nome_completo;
```

Neste caso, o valor da variável `$primeiro_nome` substituirá o primeiro marcador de posição (%s) e o valor da variável `$sobrenome` substituirá o segundo marcador de posição (%s), resultando na saída "Olá, meu nome é Maria Gomes".

## Profundidade

A concatenação de strings podem ser feitas de diversas maneiras e é importante entender quando usar cada uma delas. Além disso, também é importante estar ciente de que a concatenação de strings pode ser um pouco mais lenta do que outras formas de manipulação de strings. Portanto, é sempre bom testar e encontrar a melhor opção para o seu código.

Outra dica importante é sempre ter cuidado com a formatação da string concatenada, pois erros podem ocorrer se não houver espaços ou símbolos de concatenação corretamente. Além disso, tome cuidado para não exceder o limite de memória ao manipular grandes quantidades de strings.

## Veja também

- [Documentação oficial do PHP sobre concatenação de strings](https://www.php.net/manual/en/language.operators.string.php)
- [Tutorial sobre concatenação de strings no PHP](https://www.tutorialrepublic.com/php-tutorial/php-string-concatenation.php)
- [Dicas para otimizar a concatenação de strings em PHP](https://www.slimframework.com/docs/v3/cookbook/fast-strings.html)