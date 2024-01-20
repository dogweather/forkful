---
title:                "Analisando uma data a partir de uma string"
html_title:           "PowerShell: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Analisando Datas em Haskell: Uma Visão Prática

## O Que & Por Que?
Analisar uma data a partir de uma string significa converte-la para um tipo de data reconhecível. Programadores fazem isso para poder manipular datas de modo inteligente, como calcular diferenças ou formatar de diferentes maneiras.

## Como fazer:
Para essa tarefa, usaremos a biblioteca Time em Haskell. Em primeiro lugar, importamos o que precisamos:

```Haskell
import Data.Time
```

Agora, vamos utilizar a função `parseTimeM` para converter uma string para um objeto `Day`.

```Haskell
parseDate :: String -> Maybe Day
parseDate str = parseTimeM True defaultTimeLocale "%Y-%m-%d" str

-- Testando a função
main = print (parseDate "2020-04-25")
```

Isso irá imprimir `Just 2020-04-25`.

## Mergulho Profundo
Historicamente, a análise de datas era uma tarefa complexa devido à falta de padronização. Hoje, a ISO 8601 é geralmente a norma adotada, da qual "YYYY-MM-DD" é um exemplo.

Em Haskell, poderíamos usar outras bibliotecas, como "date" ou "old-locale", mas "time" é a mais comumente usada e é bastante eficiente.

Vimos o uso de `parseTimeM` com um formato fixo. Mas você também pode alterá-lo de acordo com suas necessidades. A função `parseTimeM` usa uma especificação de formato para fazer a análise. Por exemplo, "%Y-%m-%d" é para o formato de data YYYY-MM-DD. Você pode alterá-lo de acordo com o formato desejado.

## Veja também
Para uma imersão mais profunda nesse tópico, confira os seguintes links:

1. Documentação da biblioteca Time: http://hackage.haskell.org/package/time-1.11.1.1/docs/Data-Time.html
2. Um tutorial completo sobre tratamento de datas e tempo em Haskell: https://wiki.haskell.org/Dealing_with_data_and_time_in_Haskell
3. Padrões de especificação de formato: https://strftime.org/