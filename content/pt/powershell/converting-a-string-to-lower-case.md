---
title:                "Convertendo uma string para minúsculas"
html_title:           "PowerShell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Sobre o que? E por que?

Converter uma string para letras minúsculas é um processo em que a caixa alta de todas as letras em uma determinada cadeia de caracteres é alterada para letras minúsculas. Isso pode ser feito por diversos motivos, incluindo padronização de dados, facilitar a comparação de strings e tornar a saída mais legível.

## Como fazer:

```PowerShell
# Exemplo de uma string em letras maiúsculas
$string = "MEU TEXTO EM LETRAS MAIÚSCULAS"

# Utilizando o comando ToLower() para converter para letras minúsculas
$string.ToLower()

# Saída:
meu texto em letras maiúsculas
```

## Profundidade:

O processo de conversão de strings para letras minúsculas tem sido utilizado desde os primórdios da programação. Ele é particularmente útil em linguagens de programação case-sensitive, onde é necessário diferenciar entre letras maiúsculas e minúsculas na hora de comparar strings. Existem alternativas para esse processo, como a utilização de expressões regulares ou funções específicas de cada linguagem, mas o uso do comando ToLower() é a forma mais simples e eficaz de converter strings para letras minúsculas em PowerShell.

## Veja também:

- Documentação oficial do comando ToLower(): https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/tolower
- Mais informações sobre strings em PowerShell: https://devblogs.microsoft.com/scripting/powershell-string-fundamentals-understanding-quoting-rules-operators-and-more/