---
title:                "Elm: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string para letras minúsculas?

Ao trabalhar com programação, especialmente em Elm, é comum nos depararmos com códigos que requerem que os textos sejam padronizados em letras minúsculas. Isso pode ser necessário para fazer buscas mais precisas, comparações de strings, ou simplesmente para deixar o texto mais legível. Felizmente, em Elm há uma maneira simples de converter uma string para letras minúsculas, tornando esse processo rápido e eficiente.

## Como fazer?

Para converter uma string para letras minúsculas em Elm, utilizamos a função `String.toLower`. Essa função recebe como argumento uma string e retorna o mesmo texto, porém com todas as letras minúsculas. Veja um exemplo de código abaixo:

```Elm
nomeCompleto = "JOÃO SILVA"
nomeMin = String.toLower nomeCompleto
```

Nesse exemplo, a variável `nomeMin` irá conter a string "joão silva". Podemos também encadear essa função com outras operações, como por exemplo juntar strings, para formar um nome completo em letras minúsculas:

```Elm
nomeCompletoMin = String.join " " (String.toLower "JOÃO") (String.toLower "SILVA")
```

O resultado seria a string "joão silva", separada por um espaço.

## Aprofundando um pouco mais

É importante lembrar que a função `String.toLower` irá converter apenas as letras da string para minúsculas, não afetando outros caracteres, como números ou pontuações. Em alguns casos, pode ser necessário criar uma função personalizada, utilizando a função `String.foldl`, para garantir que todos os caracteres sejam convertidos. Além disso, é preciso estar atento ao idioma utilizado, uma vez que cada língua possui suas particularidades na hora de converter para letras minúsculas.

## Ver também

- [Documentação oficial do Elm sobre a função String.toLower](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- [Explicação em vídeo de como converter strings para letras minúsculas em Elm](https://www.youtube.com/watch?v=B3MDtKgUN8E)
- [Código no GitHub com uma função personalizada de conversão para letras minúsculas em diversos idiomas](https://github.com/JesterXL/elm-string-utils/blob/master/src/StringConversions.elm)