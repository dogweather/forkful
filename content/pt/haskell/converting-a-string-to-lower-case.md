---
title:                "Haskell: Convertendo uma string para minúsculas"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string para minúsculas?
Converter uma string para letras minúsculas é útil quando se deseja padronizar o formato de entrada de dados em um programa, especialmente para evitar erros de comparação ou busca de strings. Também pode ser útil para fins de formatação de saída de dados.

## Como fazer
Para converter uma string para letras minúsculas em Haskell, podemos usar a função `map` juntamente com a função `toLower` do módulo `Data.Char`. Aqui está um exemplo de código:

```Haskell
import Data.Char (toLower)

toLowerString :: String -> String
toLowerString str = map toLower str

main = do
  let inputString = "TEXTO EM MAIÚSCULAS"
  let outputString = toLowerString inputString
  putStrLn outputString
```

O resultado desta função será "texto em maiúsculas", como esperado.

## Mergulho profundo
Em Haskell, strings são listas de caracteres. Portanto, a função `map` é aplicada a cada caractere da string usando a função `toLower`, convertendo-os para letras minúsculas. Vale ressaltar que `toLower` só funciona para caracteres latinos, portanto, outros caracteres podem não ser convertidos corretamente.

Uma outra maneira de converter uma string para letras minúsculas em Haskell é usando a função `toLower` do módulo `Data.Text`. Aqui está um exemplo de código:

```Haskell
import Data.Text (toLower)

toLowerString :: String -> String
toLowerString str = toLower (pack str)

main = do
  let inputString = "Texto em maiúsculas"
  let outputString = toLowerString inputString
  putStrLn outputString
```

Este método também produzirá o resultado "texto em maiúsculas", mas é mais eficiente, especialmente para strings longas.

## Veja também
- [Documentação do módulo Data.Char](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)
- [Documentação do módulo Data.Text](https://hackage.haskell.org/package/text-1.2.2.2/docs/Data-Text.html)
- [Tutorial de Haskell no devHaskell](https://www.devhaskell.com/2011/08/toturial-basico-de-haskell-parte-3-letras-en-minusculas)