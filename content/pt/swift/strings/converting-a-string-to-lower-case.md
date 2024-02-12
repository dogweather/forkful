---
title:                "Convertendo uma string para minúsculas"
aliases:
- /pt/swift/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:39:31.410353-07:00
model:                 gpt-4-1106-preview
simple_title:         "Convertendo uma string para minúsculas"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Converter uma string para minúsculas significa transformar todos os caracteres alfabéticos do texto para a sua forma minúscula. Programadores fazem isso para padronizar os dados, facilitar comparações e buscas insensíveis a maiúsculas/minúsculas.

## Como Fazer:
Swift torna o processo de converter strings para minúsculas muito simples com o uso da propriedade `lowercased`. Aqui tem um exemplo:

```Swift
let fraseOriginal = "Olá Mundo!"
let fraseMinúscula = fraseOriginal.lowercased()
print(fraseMinúscula)
```

Saída do exemplo:
```
olá mundo!
```

## Mergulho Profundo:
Historicamente, a necessidade de converter strings para minúsculas remonta aos primeiros dias da computação, quando sistemas diferenciavam entre maiúsculas e minúsculas (case-sensitive). Embora isso ainda aconteça em muitos contextos, a conversão para minúsculas ajuda a uniformizar os dados.

Alternativas para a conversão incluem o uso de métodos como `uppercaseString` para transformar em maiúsculas ou `capitalizedString` para capitalizar cada palavra, dependendo do objetivo desejado.

No Swift, a implementação detalhada da conversão para caixa baixa leva em conta convenções de localização e idioma (locale). Assim sendo, caracteres específicos de certos idiomas são convertidos apropriadamente, garantindo uma maior precisão e respeito por particularidades culturais.

## Veja Também:
Para mais detalhes sobre strings em Swift e manipulações relacionadas, essas são algumas fontes úteis:
- Documentação oficial da Apple sobre strings: [Swift String](https://developer.apple.com/documentation/swift/string)
- Perguntas e respostas sobre manipulação de strings em Swift no Stack Overflow: [Stack Overflow: Swift String](https://stackoverflow.com/questions/tagged/swift+string)
