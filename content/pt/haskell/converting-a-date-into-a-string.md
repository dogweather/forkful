---
title:    "Haskell: Convertendo uma data em uma string"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Conversão de dados é um aspecto crucial em qualquer linguagem de programação, e o Haskell não é exceção. Ao converter uma data em uma string, podemos exibir a data de uma forma mais legível e amigável para os usuários. Além disso, essa habilidade pode ser útil em várias aplicações, como sistemas de gerenciamento de eventos ou aplicativos de calendário.

## Como fazer a conversão

Para converter uma data em uma string em Haskell, podemos utilizar a função `formatTime` da biblioteca `Data.Time.Format`. Vamos dar uma olhada em um exemplo simples:

```Haskell
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (getCurrentTime)

main = do
    currentTime <- getCurrentTime
    let format = "%d/%m/%Y"
    let dateStr = formatTime defaultTimeLocale format currentTime
    print dateStr

-- Output: "06/05/2021"
```

Neste exemplo, primeiro importamos as bibliotecas necessárias e, em seguida, usamos a função `getCurrentTime` para obter a data e hora atuais. Em seguida, definimos o formato da string que queremos gerar e aplicamos a função `formatTime` para converter a data atual em uma string no formato especificado. Por fim, imprimimos a string resultante.

Você também pode usar a função `parseTimeM` para fazer a conversão inversa - de uma string para uma data. Confira a [documentação](https://hackage.haskell.org/package/time/docs/Data-Time-Format.html) para mais detalhes.

## Mergulho profundo

A função `formatTime` pode ser um pouco confusa no início, pois possui muitos parâmetros opcionais. Além disso, o formato de data e hora é definido de forma diferente no Haskell em comparação com outras linguagens. É importante lembrar que todos os parâmetros são do tipo `String` e alguns deles têm letras maiúsculas e minúsculas diferentes para representar diferentes unidades de tempo. Por exemplo, `%a` representa o dia da semana abreviado (ex: "Seg") e `%A` representa o dia da semana por extenso (ex: "Segunda-feira").

Além disso, a formatação também pode mudar dependendo do local onde o código está sendo executado. No nosso exemplo, usamos `defaultTimeLocale`, mas você pode especificar um local específico usando a função `mkTimeLocale` e passá-lo como um parâmetro para `formatTime`.

## Veja também

- [Documentação da biblioteca Data.Time.Format](https://hackage.haskell.org/package/time/docs/Data-Time-Format.html)
- [Tutorial Codeacademy sobre formatação de datas em Haskell](https://www.codecademy.com/learn/learn-haskell/modules/intro-to-haskell/time)
- [Vídeo tutorial da série "Haskell do Básico ao Avançado" sobre manipulação de datas e horas](https://www.youtube.com/watch?v=lBDhnFOlUM8)