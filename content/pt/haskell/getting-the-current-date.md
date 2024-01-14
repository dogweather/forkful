---
title:    "Haskell: Obtendo a data atual"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# Por que?

Há muitas vezes em que precisamos saber a data atual para realizar certas tarefas em nossos programas. Pode ser para gerar relatórios com a data atual, fazer cálculos com datas ou simplesmente mostrar a data para o usuário.

# Como Fazer

Para obter a data atual em Haskell, podemos usar a função `getCurrentTime` do pacote `Data.Time`. Primeiro, precisamos importar o módulo com a declaração `import Data.Time`, em seguida, podemos utilizar a função da seguinte forma:

```Haskell
import Data.Time

dataAtual <- getCurrentTime
```

Assim, a variável `dataAtual` irá conter um objeto do tipo `UTCTime` que representa a data e hora atual. Podemos então utilizar funções do módulo `Data.Time.Format` para formatar essa data da maneira desejada. Por exemplo, podemos imprimir a data no formato "dia/mês/ano":

```Haskell
import Data.Time
import Data.Time.Format

dataAtual <- getCurrentTime
let dataFormatada = formatTime defaultTimeLocale "%d/%m/%Y" dataAtual

print dataFormatada -- Saída: 25/06/2021
```

# Mergulho Profundo

A função `getCurrentTime` retorna a data atual em formato UTC (Coordinated Universal Time). Para trabalhar com datas em outros fusos horários, podemos utilizar a função `getCurrentTimeZone` do mesmo módulo `Data.Time`. Essa função nos permite obter o objeto `TimeZone` correspondente ao fuso horário atual do sistema. Em seguida, podemos converter a data UTC para esse fuso utilizando a função `utcToLocalTime`.

Além disso, também podemos realizar operações com datas utilizando as funções do módulo `Data.Time.Calendar`. Por exemplo, para adicionar um dia à data atual, podemos utilizar a função `addDays`:

```Haskell
import Data.Time
import Data.Time.Calendar

dataAtual <- getCurrentTime
let umDiaDepois = addDays 1 $ utctDay dataAtual

print umDiaDepois -- Saída: 2021-06-26
```

# Veja Também

- [Documentação do pacote `Data.Time`](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Tutorial de Haskell no Wikibooks](https://en.wikibooks.org/wiki/Haskell)
- [Fórum brasileiro de Haskell](https://groups.google.com/g/haskell-br)