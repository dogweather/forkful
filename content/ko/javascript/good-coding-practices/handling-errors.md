---
date: 2024-01-26 00:55:38.897077-07:00
description: "\uC5B4\uB5BB\uAC8C \uD560\uAE4C: \uC544\uB798\uB294 \uD074\uB798\uC2DD\
  \uD55C `try-catch` \uBE14\uB85D\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.801384-06:00'
model: gpt-4-1106-preview
summary: "\uC544\uB798\uB294 \uD074\uB798\uC2DD\uD55C `try-catch` \uBE14\uB85D\uC785\
  \uB2C8\uB2E4."
title: "\uC5D0\uB7EC \uCC98\uB9AC\uD558\uAE30"
weight: 16
---

## 어떻게 할까:
아래는 클래식한 `try-catch` 블록입니다:

```javascript
try {
  // 에러가 발생할 수 있는 코드
  let result = potentiallyRiskyOperation();
  console.log('성공:', result);
} catch (error) {
  // 에러가 발생했을 때 수행할 작업
  console.error('이런:', error.message);
}
```

에러가 발생하지 않았을 때의 샘플 출력:
```
성공: 42
```

에러가 발생했을 때:
```
이런: 무언가 잘못됐어요
```

비동기 코드에서는, 프로미스가 관련되어 있을 때, `async` 함수 안에서 `try-catch`를 사용하세요:

```javascript
async function fetchData() {
  try {
    let data = await fetch('https://api.example.com/data');
    console.log('데이터를 가져왔습니다:', data);
  } catch (error) {
    console.error('데이터를 가져오는 데 에러가 발생했습니다:', error.message);
  }
}

fetchData();
```

## 깊이 알아보기
자바스크립트에서의 에러 처리는 진화해왔습니다. 과거에는 (ES3, 1999년 경) `try-catch` 블록만 있었습니다. 그다지 융통성이 높지는 않았지만 일단은 그 일을 해냈습니다.

ES6(2015)는 프로미스(Promises)를 도입해 `.then()`과 `.catch()`를 제공하면서 비동기 에러를 더 우아하게 처리할 수 있게 되었습니다.

```javascript
fetch('https://api.example.com/data')
  .then(data => console.log('데이터를 가져왔습니다:', data))
  .catch(error => console.error('데이터를 가져오는 데 에러가 발생했습니다:', error.message));
```

구현 세부 사항으로, 에러가 발생하면 자바스크립트 엔진은 `message`와 `stack` 같은 유용한 속성들을 가진 `Error` 객체를 생성합니다. 더 복잡한 앱을 위해서는 `Error` 클래스를 확장하여 커스텀 에러 타입을 만들 수도 있습니다 – 유용한 팁이지요.

대안들? 에러 처리를 무시하기(나쁜 아이디어), 에러-첫 번째 파라미터를 사용하는 콜백(callbacks)을 활용하기(Node.js 스타일이라 하죠), 또는 더 세련된 라이브러리와 프레임워크를 사용하여 그들만의 방식을 제공하는 것들이 있습니다.

## 또한 보기
에러 처리에 대해 더 알아보려면:

- try-catch에 관한 MDN: [MDN try...catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- Async/Await: [MDN async function](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function)
- 프로미스 가이드: [MDN Promises](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
- 커스텀 에러 생성 및 던지기: [MDN Error](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error)
