---
title:                "Encontrando o comprimento de uma sequência de caracteres"
html_title:           "PowerShell: Encontrando o comprimento de uma sequência de caracteres"
simple_title:         "Encontrando o comprimento de uma sequência de caracteres"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O que e por que?

Se você está trabalhando com strings (cadeias de caracteres) em seu código, uma das tarefas mais comuns é encontrar o seu comprimento. Isso é simplesmente contar quantos caracteres compõem uma string, incluindo espaços em branco. Programadores geralmente precisam fazer isso para realizar validações ou manipulações em seus dados.

## Como fazer:

```PowerShell
# Exemplo 1: Usando o método .Length
$string = "Olá, mundo!"
$string.Length # Output: 12

# Exemplo 2: Usando o operador de comprimento de String
$string = "I love PowerShell!"
[string]$string.Length # Output: 18
```

## Mergulho profundo:

Históricamente, encontrar o comprimento de uma string era considerado relativamente complexo. Antes do surgimento de linguagens de programação modernas, os desenvolvedores precisavam implementar suas próprias funções para realizar essa tarefa. Hoje, na maioria das linguagens de programação, há métodos e operadores disponíveis para isso.

Alternativamente, existem algumas abordagens menos comuns para encontrar o comprimento de uma string. Uma delas é usando a função de `IndexOf` para procurar o último caractere na string e retornar sua posição. No entanto, essa abordagem tende a ser mais lenta e menos precisa do que os métodos e operadores mencionados anteriormente.

No PowerShell, o comprimento de uma string é determinado pela sua propriedade `Length` ou pelo operador `Length` de string. Você pode acessá-los facilmente em seu código, sem a necessidade de implementar suas próprias funções.

## Veja também:

- [Documentação oficial do PowerShell sobre strings](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_strings?view=powershell-7)
- [Exemplos de utilização do método .Length](https://social.technet.microsoft.com/wiki/contents/articles/19719.powershell-string-methods-length.aspx)
- [PowerShell String TechNet Wiki](https://social.technet.microsoft.com/wiki/contents/articles/21027.powershell-string.aspx)