---
title:    "PHP: Convertendo uma string para minúsculas"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por que converter uma string em letras minúsculas?

A conversão de uma string para letras minúsculas pode ser útil em várias situações, por exemplo, na validação de dados de entrada ou na padronização de informações em um banco de dados. Além disso, a maioria das funções de busca e comparação de strings no PHP consideram a diferença entre letras minúsculas e maiúsculas, então converter tudo para minúsculas pode facilitar essa tarefa.

## Como converter uma string em letras minúsculas

Podemos usar a função `strtolower()` para converter uma string em letras minúsculas no PHP. Ela recebe como argumento a string a ser convertida e retorna uma nova string com todas as letras em minúsculo. Veja um exemplo abaixo:

```PHP
$texto = "Olá, Mundo!";
echo strtolower($texto); // saída: "olá, mundo!"
```

Outra forma de realizar essa conversão é utilizando o operador de atribuição composto `.=` em conjunto com a função `strtolower()`. Dessa forma, podemos manter a mesma variável com o valor já convertido para minúsculas. Veja:

```PHP
$texto = "Olá, Mundo!";
$texto .= strtolower($texto); // agora $texto tem o valor "olá, mundo!"
```

## Aprofundando na conversão de strings para minúsculas

É importante ter em mente que a função `strtolower()` no PHP considera as regras de conversão de caracteres da localidade do servidor em que o código está sendo executado. Por exemplo, ao executar esse código em um servidor com localidade em pt_BR, a string "Olá, Mundo!" será convertida para "olá, mundo!", já em um servidor com localidade em en_US, ela se tornará "olá, mundo!".

Além disso, essa função se aplica apenas à conversão de letras latinas, ou seja, caracteres como acentos e letras de outros alfabetos não serão convertidos para suas versões em minúsculas. Para isso, é necessário utilizar funções de conversão específicas para cada tipo de caractere.

## Veja também

- [Documentação oficial do PHP sobre a função strtolower()](https://www.php.net/manual/pt_BR/function.strtolower.php)
- [Exemplo prático de conversão de strings em minúsculas](https://www.php.net/manual/pt_BR/function.strtolower.php#exemplo-3883)
- [Outras funções de conversão de strings no PHP](https://www.php.net/manual/pt_BR/ref.strings.php)