---
title:                "Escrevendo testes"
aliases:
- /pt/javascript/writing-tests.md
date:                  2024-02-03T19:31:13.270606-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escrevendo testes"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?

Escrever testes em JavaScript refere-se à prática de criar scripts automatizados que executam o seu código para garantir que ele se comporte conforme esperado, o que pode melhorar significativamente a confiabilidade e a capacidade de manutenção de suas aplicações. Os programadores fazem isso para capturar bugs precocemente, facilitar a refatoração do código e garantir que novos recursos não quebrem a funcionalidade existente.

## Como fazer:

### Abordagem Nativa (usando Jest)

Jest é um framework de testes popular que fornece uma API amigável para escrever testes de unidade em JavaScript. Ele requer configuração mínima e vem com recursos como funções de simulação, temporizadores e teste de snapshots.

1. **Instalação**:

```bash
npm install --save-dev jest
```

2. **Escrevendo um teste simples**:

Crie um arquivo chamado `sum.test.js`:

```javascript
const sum = require('./sum'); // Assuma que essa função simplesmente adiciona dois números

test('soma 1 + 2 para igualar 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

3. **Executando o seu teste**:

```bash
npx jest
```

**Saída de Exemplo:**

```plaintext
PASS  ./sum.test.js
✓ soma 1 + 2 para igualar 3 (5ms)
```

### Testando Código Assíncrono

Jest facilita o teste de promessas e a sintaxe async/await:

```javascript
// asyncSum.js
async function asyncSum(a, b) {
  return Promise.resolve(a + b);
}

// asyncSum.test.js
test('adição assíncrona funciona', async () => {
  await expect(asyncSum(1, 2)).resolves.toBe(3);
});

```

### Usando Bibliotecas de Terceiros (Mocha & Chai)

Mocha é outro framework de testes popular, frequentemente usado com a biblioteca de afirmação Chai para testes mais expressivos.

1. **Instalação**:

```bash
npm install --save-dev mocha chai
```

2. **Escrevendo um teste com Mocha e Chai**:

Crie `calculate.test.js`:

```javascript
const chai = require('chai');
const expect = chai.expect;

const calculate = require('./calculate'); // Um módulo de cálculo simples

describe('Calculate', function() {
  it('deve somar dois valores', function() {
    expect(calculate.sum(5, 2)).to.equal(7);
  });
});
```

3. **Executando seus testes com Mocha**:

Adicione um script no seu `package.json`:

```json
"scripts": {
  "test": "mocha"
}
```

Em seguida, execute:

```bash
npm test
```

**Saída de Exemplo:**

```plaintext
  Calculate
    ✓ deve somar dois valores


  1 passing (8ms)
```

Estes exemplos ilustram a escrita básica e execução de testes em JavaScript. Adotar um framework de testes como Jest ou Mocha com Chai pode fornecer uma base sólida para testes robustos de aplicação, ajudando a assegurar que seu código funcione conforme pretendido através de atualizações e refatorações.
