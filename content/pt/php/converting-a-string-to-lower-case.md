---
title:                "Convertendo uma string para minúsculas"
html_title:           "Fish Shell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O Que & Por quê?

Converter uma string para minúsculo é um processo de transformação de todos os caracteres alfabéticos em uma string para lowercase (minúsculo). Programadores costumam fazer isso para normalizar dados e manter a consistência, especialmente ao comparar strings.

## Como fazer:

Aqui está um exemplo de como converter uma string para lower case em PHP:

```PHP
$originalString = "Olá Mundo!";

$lowercaseString = strtolower($originalString);

echo $lowercaseString;
```

Este código imprimirá:

```PHP
"olá mundo!"
```

## Mergulho Profundo

Historicamente no PHP, a função `strtolower()` tem sido usada para converter strings para lowercase. Ela faz parte do núcleo do PHP desde a versão 4. 

Uma alternativa à `strtolower()` é a função `mb_strtolower()`, que é mais adequada para strings que contêm caracteres não ASCII. 

```PHP
$originalString = "Olá Mundo!";

$lowercaseString = mb_strtolower($originalString, 'UTF-8');

echo $lowercaseString;
```

Implementar a conversão para minúsculo em PHP é simples, pois as funções `strtolower()` e `mb_strtolower()` fazem o trabalho pesado para nós. Tudo o que precisamos fazer é passar a string que queremos converter como parâmetro para a função.

## Veja Também

Aqui estão alguns links para documentação relevante e tutoriais:

1. [Documentação oficial do PHP para strtolower](https://www.php.net/manual/en/function.strtolower.php)
3. [mb_strtolower - Manual do PHP](https://www.php.net/manual/pt_BR/function.mb-strtolower.php)