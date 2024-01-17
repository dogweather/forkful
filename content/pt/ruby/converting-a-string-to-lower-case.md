---
title:                "Convertendo uma cadeia de caracteres para letras minúsculas"
html_title:           "Ruby: Convertendo uma cadeia de caracteres para letras minúsculas"
simple_title:         "Convertendo uma cadeia de caracteres para letras minúsculas"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O que é e por que fazer?
Converter uma string para minúsculas é um processo em que todas as letras maiúsculas em uma string são convertidas para letras minúsculas. Programadores geralmente fazem isso para padronizar as strings em seu código e torná-las mais fáceis de comparar.

## Como fazer:
Existem várias maneiras de converter uma string para minúsculas em Ruby. Aqui estão alguns exemplos:

```Ruby
# Método 1:
string = "EXEMPLO"
puts string.downcase
# Saída: exemplo

# Método 2:
string = "OUTRO EXEMPLO"
puts string.downcase!
# Saída: outro exemplo (a string original foi alterada)

# Método 3:
string = "MAIS UM EXEMPLO"
puts string.mb_chars.downcase.to_s
# Saída: mais um exemplo
```

## Profundando:
O método downcase foi introduzido na versão 1.9 do Ruby e é uma abreviação do método downcase!. Este método não é destrutivo, ou seja, ele não altera a string original, mas retorna uma nova string com letras minúsculas. O método downcase! é destrutivo e altera a string original.

Outra maneira de converter uma string para minúsculas é usando o método mb_chars, que lida melhor com caracteres multibyte. Este método é mais útil quando estamos trabalhando com strings em idiomas que utilizam caracteres especiais.

## Veja também:
- Documentação do Ruby sobre o método downcase: https://ruby-doc.org/core-2.7.1/String.html#method-i-downcase
- Documentação do Ruby sobre o método mb_chars: https://ruby-doc.org/core-2.7.1/ActiveSupport/Multibyte/Chars.html#method-i-downcase