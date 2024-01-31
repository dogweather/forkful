---
title:                "Removendo aspas de uma string"
date:                  2024-01-26T03:42:16.877069-07:00
model:                 gpt-4-0125-preview
simple_title:         "Removendo aspas de uma string"

category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Remover aspas de uma string significa eliminar os caracteres de aspas simples (`'`) ou duplas (`"`) que delimitam os literais de strings no código. Programadores fazem isso por várias razões, como formatar saídas, higienizar entradas de usuários ou preparar strings para análise ou armazenamento onde as aspas são desnecessárias ou podem causar erros.

## Como fazer:
Aqui está o seu guia direto para livrar suas strings dessas irritantes marcas de aspas no TypeScript.

```typescript
// Opção A: Substituir aspas simples ou duplas usando regex
function removeQuotes(input: string): string {
  return input.replace(/^["']|["']$/g, '');
}

console.log(removeQuotes(`"String com aspas"`)); // String com aspas
console.log(removeQuotes(`'Outra string'`)); // Outra string

// Opção B: Lidar com strings que começam e terminam com aspas diferentes
function removeMismatchedQuotes(input: string): string {
  return input.replace(/^(['"])(.*?)(?<!\1)\1$/, '$2');
}

console.log(removeMismatchedQuotes(`"Aspas desencontradas'`)); // "Aspas desencontradas'

// Opção C: Remover vários tipos de aspas
function removeAllQuotes(input: string): string {
  return input.replace(/['"]+/g, '');
}

console.log(removeAllQuotes(`"'Mistura'n'Match'"`)); // Mistura'n'Match
```

## Aprofundamento
Muito antes do TypeScript sequer existir, programadores de JavaScript já lidavam com as shenanigans das aspas, e a história é praticamente a mesma para o TypeScript. À medida que os tempos mudam, também muda a maneira como recortamos strings. Hoje em dia, com o poder muscular dos regex, deixamos de lado métodos tediosos de fatiamento de strings ou outros.

Embora os exemplos acima devam cobrir a maioria de suas necessidades, lembre-se, citar pode ficar complexo. Aspas aninhadas, desencontradas e escapadas são as traiçoeiras esperando para te fazer tropeçar. Para essas, você pode precisar de padrões mais sofisticados ou até mesmo de parsers para lidar com cada caso complicado.

Alternativas? Algumas pessoas gostam de usar bibliotecas como lodash, com métodos como `trim` e `trimStart` / `trimEnd`, que podem ser adaptados para cortar aspas se você definir os caracteres que deseja aparar.

E para você, entusiasta do TypeScript, não vamos esquecer dos tipos. Embora aqui estejamos lidando principalmente com strings, quando você está trabalhando com entrada de usuários ou análise, lançar mão de guardas de tipo ou mesmo de genéricos pode ajudar a garantir que você mantenha seu código tão seguro quanto suas aspas são aparadas.

## Veja Também
Confira estes pontos virtuais para mais informações:

- MDN Web Docs sobre regex (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- Documentação Oficial do TypeScript (https://www.typescriptlang.org/docs/)
- You Don't Need Lodash/Underscore – Helpers de String (https://github.com/you-dont-need/You-Dont-Need-Lodash-Underscore#strings)
- Stack Overflow: Percorra as trincheiras onde inúmeros devs lutaram contra catástrofes de aspas (https://stackoverflow.com/)
