---
title:                "Tratamento de erros"
aliases:
- /pt/javascript/handling-errors/
date:                  2024-01-26T00:54:25.526434-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tratamento de erros"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/handling-errors.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?

O tratamento de erros é como você gerencia quando as coisas não saem como esperado no seu código. É crucial porque ajuda seus programas a falharem de maneira controlada e instrui os usuários de forma clara, em vez de apenas travarem e falharem.

## Como fazer:

Aqui está o clássico bloco `try-catch`:

```javascript
try {
  // Código que pode lançar um erro
  let result = potentiallyRiskyOperation();
  console.log('Sucesso:', result);
} catch (error) {
  // O que fazer se um erro for lançado
  console.error('Ops:', error.message);
}
```

Saída de exemplo quando nenhum erro ocorre:
```
Sucesso: 42
```

E quando há um erro:
```
Ops: Algo deu errado
```

Para código assíncrono, onde promessas estão envolvidas, use `try-catch` em uma função `async`:

```javascript
async function fetchData() {
  try {
    let data = await fetch('https://api.example.com/data');
    console.log('Dados obtidos:', data);
  } catch (error) {
    console.error('Erro ao obter dados:', error.message);
  }
}

fetchData();
```

## Aprofundando

O tratamento de erros em JavaScript evoluiu. Lá atrás (ES3, cerca de 1999), tínhamos apenas o bloco `try-catch`. Não era super flexível, mas fazia o trabalho.

ES6 (2015) introduziu Promessas e nos deu `.then()` e `.catch()`, permitindo-nos tratar erros assíncronos de forma mais elegante.

```javascript
fetch('https://api.example.com/data')
  .then(data => console.log('Dados obtidos:', data))
  .catch(error => console.error('Erro ao obter dados:', error.message));
```

Quanto aos detalhes de implementação, quando um erro é lançado, os motores JavaScript criam um objeto `Error` com propriedades úteis como `message` e `stack`. Você também pode criar tipos de erros personalizados estendendo a classe `Error` – útil para aplicativos mais complexos.

Alternativas? Você poderia ignorar o tratamento de erros (má ideia), usar callbacks com parâmetros que priorizam erros (olá, estilo Node.js), ou ser mais sofisticado com bibliotecas e frameworks que oferecem suas próprias soluções.

## Veja Também

Para mais informações sobre tratamento de erros:

- MDN sobre try-catch: [MDN try...catch](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Statements/try...catch)
- Async/Await: [MDN função async](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Statements/async_function)
- Um guia para Promessas: [MDN Promessas](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Promise)
- Criando e lançando erros personalizados: [MDN Error](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Error)
