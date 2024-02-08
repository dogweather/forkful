---
title:                "Removendo aspas de uma string"
aliases:
- pt/elm/removing-quotes-from-a-string.md
date:                  2024-01-26T03:38:50.875567-07:00
model:                 gpt-4-0125-preview
simple_title:         "Removendo aspas de uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Remover aspas de uma string significa eliminar aquelas aspas duplas ou simples extras que você não precisa no texto processado. Programadores fazem isso para higienizar a entrada, preparar dados para armazenamento ou tornar a saída mais legível para humanos quando as aspas não são necessárias para o contexto dado.

## Como Fazer:
No Elm, você pode usar as funções de `String` para manipular strings, como remover aspas. Aqui está uma maneira direta de fazer isso:

```Elm
removeQuotes : String -> String
removeQuotes str =
    String.trim (String.filter (\char -> char /= '\"' && char /= '\'') str)

main =
    String.removeQuotes "\"Esta é uma string 'com aspas'!\""
    -- Saída: Esta é uma string com aspas!
```

Apenas lembre-se: esse pequeno trecho de código removerá todas as aspas da sua string, então use-o com sabedoria!

## Aprofundamento
Antigamente, lidar com strings era um pouco mais manual, envolvendo muita análise minuciosa. Hoje em dia, linguagens como Elm tornam isso mais simples com funções integradas. A função `String.filter` é uma ferramenta versátil no seu arsenal para quando você precisa se preocupar com cada caracter, o que inclui, mas não se limita a, arrancar aspas.

Como alternativa, você poderia recorrer a expressões regulares se Elm as suportasse de maneira portável, o que não acontece por padrão. Mas o foco do Elm em simplicidade e segurança significa que nossa abordagem com `String.filter` é clara, segura e fácil de manter.

A abordagem funcional do Elm incentiva funções puras sem efeitos colaterais, e `removeQuotes` é um exemplo perfeito. Ela recebe uma string e retorna uma nova, deixando a original intocada. Isso mostra as estruturas de dados imutáveis do Elm em ação, promovendo previsibilidade e aliviando suas dores de cabeça com depuração.

## Veja Também
Para leituras adicionais e aventuras relacionadas à manipulação de strings, confira a documentação do módulo `String` do Elm em:

- [Documentação de String do Elm](https://package.elm-lang.org/packages/elm/core/latest/String)

E se você estiver em dúvida sobre o que o Elm suporta em termos de manipulação de strings ou qualquer funcionalidade da linguagem:

- [Guia da Linguagem Elm](https://guide.elm-lang.org/)
