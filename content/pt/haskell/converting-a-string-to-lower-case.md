---
title:                "Convertendo uma string para letras minúsculas"
html_title:           "Haskell: Convertendo uma string para letras minúsculas"
simple_title:         "Convertendo uma string para letras minúsculas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O que é e por que fazer?

Converter uma string para minúsculas é um processo comum em programação, em que todas as letras presentes em uma string são transformadas em letras minúsculas. Isso pode ser útil para padronizar a entrada do usuário, facilitando a comparação de strings ou a busca por palavras-chave. 

## Como fazer:

Você pode facilmente converter uma string para minúsculas em Haskell usando a função `toLower` do módulo` Data.Char`. Basta importar o módulo e aplicar a função à sua string desejada, como no exemplo abaixo:

```Haskell
import Data.Char

stringEmUppercase = "OLÁ, AMIGOS!"
stringEmLowercase = map toLower stringEmUppercase

print stringEmLowercase -- output: "olá, amigos!"
```

Observe que a função `map` é usada aqui para aplicar a função `toLower` a cada caractere da string. Isso nos permite converter qualquer string, independentemente do tamanho, para minúsculas.

## Profundidade:

A conversão de string para minúsculas pode ser rastreada até os primórdios da programação. Antigamente, quando o armazenamento de dados era limitado, muitos programadores escolhiam armazenar suas strings em letra maiúscula para economizar espaço de memória. No entanto, isso se tornou menos relevante com o avanço da tecnologia e hoje em dia a escolha é mais uma questão de preferência. 

Existem outras formas de converter uma string para minúsculas em Haskell, como usando funções de bibliotecas externas como `Text` e `String`, mas a função `toLower` é a maneira mais simples e comumente usada. É importante notar que essa função é sensível ao idioma e pode não funcionar corretamente para caracteres acentuados em diferentes línguas. Se esse for o caso, existem alternativas que solucionam esse problema

## Veja também:

- [Haskell Data.Char module documentation](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)
- [Haskell String library](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-String.html)
- [Haskell Text library](https://hackage.haskell.org/package/text-1.2.4.0/docs/Data-Text.html)