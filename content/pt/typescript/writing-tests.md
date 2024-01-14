---
title:                "TypeScript: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes é importante?

Escrever testes é uma parte importante do processo de desenvolvimento de software. Eles garantem que o código esteja funcionando corretamente e previnem a ocorrência de erros no futuro. Além disso, testes bem escritos também ajudam a documentar o funcionamento do código, facilitando sua manutenção e atualização.

## Como escrever testes em TypeScript

Escrever testes em TypeScript é simples e pode ser feito usando a biblioteca de testes Jest. Primeiro, instale o Jest usando o comando `npm install jest --save-dev`. Em seguida, crie um arquivo de teste com a extensão `.spec.ts` e importe a função ou classe que deseja testar. Dentro do arquivo de teste, crie uma função utilizando o método `test()` do Jest para definir o nome e o corpo do teste. Por exemplo:

```TypeScript
import { sum } from './utils'

test('Teste de soma', () => {
  expect(sum(2, 2)).toBe(4)
})
```

O código acima testa se a função `sum` do arquivo `utils.ts` retorna corretamente o resultado de 2+2. É possível adicionar quantos testes forem necessários para garantir que a função está funcionando corretamente.

## Aprofundando em escrita de testes

Escrever testes em TypeScript envolve entender alguns conceitos importantes, como mocking (simular valores de retorno de funções) e asserções (verificar valores retornados ou alterações feitas pelo código). O Jest possui uma documentação extensa que aborda esses e outros conceitos de forma detalhada. É importante também seguir boas práticas, como escrever testes independentes uns dos outros e utilizar nomes descritivos para os testes.

## Veja também
- Documentação do Jest (https://jestjs.io/docs/en/getting-started)
- Artigo "Testes em Typescript com jest" - Medium (https://medium.com/flutterando/testes-em-typescript-com-jest-ed9021236a0f?source=---------8-----------------------)