---
date: 2024-01-26 00:58:30.861944-07:00
description: "\uC5D0\uB7EC \uD578\uB4E4\uB9C1\uC774\uB780 \uC608\uC0C1\uCE58 \uBABB\
  \uD55C \uC0C1\uD669\uC744 \uB300\uBE44\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\
  \uB2E4; \uC6B0\uB9AC\uC758 \uCF54\uB4DC\uC5D0\uC11C \uBB34\uC5B8\uAC00 \uC798\uBABB\
  \uB418\uC5C8\uC744 \uB54C, \uC6B0\uB9AC\uB294 \uC5B4\uB5BB\uAC8C \uAD00\uB9AC\uD558\
  \uB294\uAC00\uC5D0 \uB300\uD55C \uAC83\uC785\uB2C8\uB2E4. \uC6B0\uB9AC\uB294 \uD504\
  \uB85C\uADF8\uB7A8\uC774 \uCDA9\uB3CC\uC744 \uD53C\uD558\uACE0 \uC0AC\uC6A9\uC790\
  \uC5D0\uAC8C \uBD80\uB4DC\uB7EC\uC6B4 \uACBD\uD5D8\uC744 \uC81C\uACF5\uD558\uAE30\
  \ \uC704\uD574\uC11C, \uC608\uC0C1\uCE58 \uBABB\uD55C \uC0C1\uD669\uC5D0\uB3C4 \uBD88\
  \uAD6C\uD558\uACE0, \uC774\uB97C \uC218\uD589\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.864004-06:00'
model: gpt-4-1106-preview
summary: "\uC5D0\uB7EC \uD578\uB4E4\uB9C1\uC774\uB780 \uC608\uC0C1\uCE58 \uBABB\uD55C\
  \ \uC0C1\uD669\uC744 \uB300\uBE44\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4\
  ; \uC6B0\uB9AC\uC758 \uCF54\uB4DC\uC5D0\uC11C \uBB34\uC5B8\uAC00 \uC798\uBABB\uB418\
  \uC5C8\uC744 \uB54C, \uC6B0\uB9AC\uB294 \uC5B4\uB5BB\uAC8C \uAD00\uB9AC\uD558\uB294\
  \uAC00\uC5D0 \uB300\uD55C \uAC83\uC785\uB2C8\uB2E4. \uC6B0\uB9AC\uB294 \uD504\uB85C\
  \uADF8\uB7A8\uC774 \uCDA9\uB3CC\uC744 \uD53C\uD558\uACE0 \uC0AC\uC6A9\uC790\uC5D0\
  \uAC8C \uBD80\uB4DC\uB7EC\uC6B4 \uACBD\uD5D8\uC744 \uC81C\uACF5\uD558\uAE30 \uC704\
  \uD574\uC11C, \uC608\uC0C1\uCE58 \uBABB\uD55C \uC0C1\uD669\uC5D0\uB3C4 \uBD88\uAD6C\
  \uD558\uACE0, \uC774\uB97C \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uC5D0\uB7EC \uCC98\uB9AC\uD558\uAE30"
weight: 16
---

## 무엇을, 왜?
에러 핸들링이란 예상치 못한 상황을 대비하는 것을 말합니다; 우리의 코드에서 무언가 잘못되었을 때, 우리는 어떻게 관리하는가에 대한 것입니다. 우리는 프로그램이 충돌을 피하고 사용자에게 부드러운 경험을 제공하기 위해서, 예상치 못한 상황에도 불구하고, 이를 수행합니다.

## 어떻게:
TypeScript에서 에러를 처리하는 것은 종종 `try`, `catch`, `finally` 블록을 포함합니다.

```typescript
function riskyOperation() {
  throw new Error("무언가 잘못되었습니다!");
}

function handleErrors() {
  try {
    riskyOperation();
  } catch (error) {
    console.error("에러가 발생했습니다:", error.message);
  } finally {
    console.log("이것은 에러 여부와 상관 없이 항상 실행됩니다.");
  }
}

handleErrors();
```

출력 예시:

```
에러가 발생했습니다: 무언가 잘못되었습니다!
이것은 에러 여부와 상관 없이 항상 실행됩니다.
```

프로미스를 이용한 비동기 예제:

```typescript
async function asyncRiskyOperation() {
  return new Promise((resolve, reject) => {
    // 에러를 시뮬레이션
    reject("참담하게 실패함");
  });
}

async function handleAsyncErrors() {
  try {
    await asyncRiskyOperation();
  } catch (error) {
    console.error("비동기 에러 발생:", error);
  }
}

handleAsyncErrors();
```

출력 예시:

```
비동기 에러 발생: 참담하게 실패함
```

## 심화 탐구
에러 핸들링은 프로그래밍이 시작된 이래로 핵심 요소였습니다. JavaScript를 기반으로 하는 TypeScript에서는, ECMAScript 2017에서 async/await이 도입됨에 따라 에러 핸들링이 더욱 견고해졌습니다. 그 전까지는 비동기 코드에서 에러를 처리하기 위해 콜백 함수와 프로미스에 주로 의존했습니다.

TypeScript에서 `try/catch`의 대안은 React와 같은 프레임워크에서 제공하는 에러 경계를 사용하는 것입니다. 서버 측 핸들링의 경우, Express.js와 같은 플랫폼에서 미들웨어를 사용하여 에러 관리를 중앙집중화할 수 있습니다.

구현면에서, TypeScript는 자체 에러 핸들링 메커니즘이 없고 JavaScript의 그것에 의존합니다. 사용자 정의 에러 클래스는 `Error` 클래스를 확장하여 더 설명적인 에러 정보를 제공할 수 있습니다.

## 참고자료
- [MDN의 try/catch에 대하여](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- [MDN의 Async/Await에 대하여](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Async_await)
- [React에서 에러 경계 사용하기](https://reactjs.org/docs/error-boundaries.html)
- [Express.js 에러 핸들링](https://expressjs.com/en/guide/error-handling.html)
