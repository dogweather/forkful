---
title:                "Obsługa błędów"
aliases:
- /pl/typescript/handling-errors.md
date:                  2024-01-26T00:58:56.810940-07:00
model:                 gpt-4-1106-preview
simple_title:         "Obsługa błędów"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/handling-errors.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Obsługa błędów polega na oczekiwaniu na nieoczekiwane; to, jak zarządzamy sytuacjami, gdy coś idzie nie tak w naszym kodzie. Robimy to, aby uniknąć awarii i zapewnić użytkownikom płynne doświadczenie, nawet kiedy zdarzy się coś nieoczekiwanego.

## Jak to zrobić:
W TypeScript obsługa błędów często wiąże się z blokami `try`, `catch` i `finally`.

```typescript
function riskyOperation() {
  throw new Error("Coś poszło nie tak!");
}

function handleErrors() {
  try {
    riskyOperation();
  } catch (error) {
    console.error("Złapany błąd:", error.message);
  } finally {
    console.log("To zawsze się wykonuje, niezależnie od błędu.");
  }
}

handleErrors();
```

Przykładowe wyjście:

```
Złapany błąd: Coś poszło nie tak!
To zawsze się wykonuje, niezależnie od błędu.
```

Przykład asynchroniczny z obietnicami:

```typescript
async function asyncRiskyOperation() {
  return new Promise((resolve, reject) => {
    // Symulacja błędu
    reject("Zakończone fiaskiem");
  });
}

async function handleAsyncErrors() {
  try {
    await asyncRiskyOperation();
  } catch (error) {
    console.error("Złapany błąd asynchroniczny:", error);
  }
}

handleAsyncErrors();
```

Przykładowe wyjście:

```
Złapany błąd asynchroniczny: Zakończone fiaskiem
```

## Dogłębna analiza
Obsługa błędów jest kamieniem węgielnym programowania od jego początków. W TypeScript, który opiera się na JavaScript, obsługa błędów stała się bardziej solidna z wprowadzeniem async/await w ECMAScript 2017. Przed tym często polegaliśmy na funkcjach zwrotnych (callback functions) i obietnicach (promises) do obsługi błędów w asynchronicznym kodzie.

Alternatywą dla `try/catch` w TypeScript jest używanie granic błędów (error boundaries), dostarczanych przez frameworki takie jak React. Do obsługi po stronie serwera możemy użyć oprogramowania pośredniczącego (middleware) na platformach takich jak Express.js, aby scentralizować zarządzanie błędami.

Pod względem implementacji, TypeScript nie ma własnego mechanizmu obsługi błędów, ale opiera się na mechanizmach JavaScript. Niestandardowe klasy błędów mogą rozszerzać klasę `Error`, oferując bardziej opisową informację o błędzie.

## Zobacz też
- [MDN o try/catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch) (MDN o try/catch)
- [Async/Await na MDN](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Async_await) (Async/Await na MDN)
- [Używanie granic błędów w React](https://reactjs.org/docs/error-boundaries.html) (Using Error Boundaries in React)
- [Obsługa błędów w Express.js](https://expressjs.com/en/guide/error-handling.html) (Express.js Error Handling)
