---
title:                "Concatenando strings"
html_title:           "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## O Que & Por Quê? 

Concatenação de strings é juntar duas ou mais strings para formar uma única. Os programadores fazem isso para manipular, reformatar e exibir dados de maneira eficiente e conveniente.

## Como fazer: 
Aqui estão dois dos métodos mais usados para concatenar strings no PowerShell:

* Uso do operador `+`

```PowerShell
$string1 = "Power"
$string2 = "Shell"
$resultado = $string1 + $string2
escreva-host $resultado
```
Output:
``` 
PowerShell
``` 

* Uso de chaves `{}` dentro de aspas duplas
```PowerShell
$string1 = "Power"
$string2 = "Shell"
$resultado = "$string1$string2"
escreva-host $resultado
```
Output:
``` 
PowerShell
```

## Deep Dive 

Historicamente, a concatenação de strings tem sido um elemento fundamental de programação e manipulação de dados. No PowerShell, a concatenação de strings é herdada de seus antecessores linguagens de script, como o Perl e o Bash.

Existem várias maneiras de concatenar strings no PowerShell, mas o uso do operador `+` e chaves {} são dois dos mais comuns. Além disso, o PowerShell suporta concatenação de string com o operador `-f` e a função `String.Format()`, ampliando a flexibilidade do programador.

Além disso, tenha cuidado ao concatenar strings em loops, pois pode ser custoso em termos de desempenho. A concatenação cria um novo objeto de string e copia o conteúdo das strings originais.

## Veja Também 

* [Documentation for about_Operators](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_operators?view=powershell-7.1)

* [Working with Strings](https://powershellexplained.com/2017-01-13-powershell-variable-substitution-in-strings)