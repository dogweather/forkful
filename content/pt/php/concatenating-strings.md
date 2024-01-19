---
title:                "Concatenando strings"
html_title:           "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Concatenar strings é a tarefa de juntar duas ou mais strings numa única. Programadores realizam isso para construir frases dinâmicas e formatar saídas de dados de maneira elegante.

## Como fazer:
Concatenar strings em PHP é simples. Você usa o operador `.` para juntar duas ou mais strings.

```PHP
$primeira = "Olá, ";
$segunda = "mundo!";
$frase = $primeira . $segunda;
echo $frase;
```

O output será:

```
Olá, mundo!
```

Você também pode usar o operador de atribuição `.=`.
```PHP
$primeira = "Olá, ";
$primeira .= "mundo!";
echo $primeira;
```

Isto irá imprimir:

```
Olá, mundo!
```

## Mergulho Profundo

A concatenação de strings está no PHP desde as suas primeiras versões. Foi pensado como uma maneira de lidar com a manipulação de texto. Existem outras maneiras de juntar strings, como a função `sprintf()` ou o uso de aspas duplas (`""`), mas a concatenação usando o operador `.` é provavelmente a mais eficiente e legível.

```PHP
$primeira = "Olá, ";
$segunda = "mundo!";
$frase = sprintf("%s%s", $primeira, $segunda);
echo $frase;
```

Resultado:
```
Olá, mundo!
```

A implementação desta funcionalidade é bastante direta na linguagem PHP. O operador `.` simplesmente une as strings lado a lado, sem adicionar ou remover nenhum caractere.

## Veja Também

Para aprender mais sobre concatenação de strings em PHP, as seguintes fontes são recomendadas:

- [Operadores de String no Manual do PHP](https://www.php.net/manual/pt_BR/language.operators.string.php)
- [Função sprintf() no Manual do PHP](https://www.php.net/manual/pt_BR/function.sprintf.php)
- [Strings no Manual do PHP](https://www.php.net/manual/pt_BR/language.types.string.php)