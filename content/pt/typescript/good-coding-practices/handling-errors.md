---
date: 2024-01-26 00:58:56.662670-07:00
description: "Tratar erros \xE9 sobre esperar o inesperado; \xE9 como gerenciamos\
  \ quando as coisas d\xE3o errado no nosso c\xF3digo. Fazemos isso para evitar falhas\
  \ e para dar aos\u2026"
lastmod: '2024-03-11T00:14:20.019958-06:00'
model: gpt-4-1106-preview
summary: "Tratar erros \xE9 sobre esperar o inesperado; \xE9 como gerenciamos quando\
  \ as coisas d\xE3o errado no nosso c\xF3digo. Fazemos isso para evitar falhas e\
  \ para dar aos\u2026"
title: Tratamento de erros
---

{{< edit_this_page >}}

## O Que & Por Quê?
Tratar erros é sobre esperar o inesperado; é como gerenciamos quando as coisas dão errado no nosso código. Fazemos isso para evitar falhas e para dar aos usuários uma experiência suave, mesmo quando o inesperado acontece.

## Como fazer:
Em TypeScript, o tratamento de erros frequentemente envolve blocos `try`, `catch` e `finally`.

```typescript
function operacaoArriscada() {
  throw new Error("Algo deu errado!");
}

function tratarErros() {
  try {
    operacaoArriscada();
  } catch (erro) {
    console.error("Erro capturado:", erro.message);
  } finally {
    console.log("Isso sempre executa, com erro ou não.");
  }
}

tratarErros();
```

Saída de exemplo:

```
Erro capturado: Algo deu errado!
Isso sempre executa, com erro ou não.
```

Exemplo assíncrono com promessas:

```typescript
async function operacaoArriscadaAsync() {
  return new Promise((resolve, reject) => {
    // Simular um erro
    reject("Falhou miseravelmente");
  });
}

async function tratarErrosAsync() {
  try {
    await operacaoArriscadaAsync();
  } catch (erro) {
    console.error("Erro assíncrono capturado:", erro);
  }
}

tratarErrosAsync();
```

Saída de exemplo:

```
Erro assíncrono capturado: Falhou miseravelmente
```

## Aprofundamento
O tratamento de erros tem sido um pilar da programação desde o seu início. No TypeScript, que se baseia em JavaScript, o tratamento de erros se tornou mais robusto com a introdução do async/await no ECMAScript 2017. Antes disso, frequentemente dependíamos de funções de retorno de chamada (callbacks) e promessas para tratar erros em código assíncrono.

Uma alternativa ao `try/catch` em TypeScript é o uso de limites de erro fornecidos por frameworks como o React. Para o tratamento do lado do servidor, podemos usar middleware em plataformas como o Express.js para centralizar a gestão de erros.

Em termos de implementação, o TypeScript não tem seu próprio mecanismo de tratamento de erros, mas depende do JavaScript. Classes de erro personalizadas podem estender a classe `Error` para oferecer informações de erro mais descritivas.

## Veja Também
- [MDN sobre try/catch](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Statements/try...catch)
- [Async/Await no MDN](https://developer.mozilla.org/pt-BR/docs/Learn/JavaScript/Asynchronous/Async_await)
- [Usando Limites de Erro no React](https://pt-br.reactjs.org/docs/error-boundaries.html)
- [Tratamento de Erros no Express.js](https://expressjs.com/pt-br/guide/error-handling.html)
