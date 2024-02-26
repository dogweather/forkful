---
date: 2024-01-26 00:58:56.810940-07:00
description: "Obs\u0142uga b\u0142\u0119d\xF3w polega na oczekiwaniu na nieoczekiwane;\
  \ to, jak zarz\u0105dzamy sytuacjami, gdy co\u015B idzie nie tak w naszym kodzie.\
  \ Robimy to, aby unikn\u0105\u0107 awarii\u2026"
lastmod: '2024-02-25T18:49:33.516097-07:00'
model: gpt-4-1106-preview
summary: "Obs\u0142uga b\u0142\u0119d\xF3w polega na oczekiwaniu na nieoczekiwane;\
  \ to, jak zarz\u0105dzamy sytuacjami, gdy co\u015B idzie nie tak w naszym kodzie.\
  \ Robimy to, aby unikn\u0105\u0107 awarii\u2026"
title: "Obs\u0142uga b\u0142\u0119d\xF3w"
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
