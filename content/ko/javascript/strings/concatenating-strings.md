---
date: 2024-01-20 17:35:20.711696-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uCD08\uCC3D\uAE30\
  \ \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uC11C\uB294 \uC8FC\uB85C '+' \uC5F0\
  \uC0B0\uC790\uAC00 \uBB38\uC790\uC5F4\uC744 \uD569\uCE58\uB294 \uB370 \uC0AC\uC6A9\
  \uB418\uC5C8\uC2B5\uB2C8\uB2E4. \uD558\uC9C0\uB9CC ES6(ES2015)\uBD80\uD130\uB294\
  \ \uD15C\uD50C\uB9BF \uB9AC\uD130\uB7F4\uC774\uB77C\uB294 \uAE54\uB054\uD55C \uBC29\
  \uBC95\uC774 \uC0DD\uACBC\uC8E0. '`' \uBB38\uC790\uB85C \uBB38\uC790\uC5F4\uC744\
  \ \uAC10\uC2F8\uACE0, `${}` \uAD6C\uBB38\uC73C\uB85C \uBCC0\uC218\uB97C \uC0BD\uC785\
  \uD569\uB2C8\uB2E4. \uBB38\uC790\uC5F4\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:10.003840-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uCD08\uCC3D\uAE30 \uC790\uBC14\
  \uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uC11C\uB294 \uC8FC\uB85C '+' \uC5F0\uC0B0\uC790\uAC00\
  \ \uBB38\uC790\uC5F4\uC744 \uD569\uCE58\uB294 \uB370 \uC0AC\uC6A9\uB418\uC5C8\uC2B5\
  \uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uD558\uAE30"
weight: 3
---

## How to: (어떻게 하나요?)
```javascript
// '+' 연산자 사용
let greeting = "안녕" + ", " + "세계!";
console.log(greeting); // "안녕, 세계!"

// 백틱(`)과 ${}를 사용한 템플릿 리터럴
let planet = "세계";
let helloWorld = `안녕, ${planet}!`;
console.log(helloWorld); // "안녕, 세계!"
```

## Deep Dive (심층 분석)
초창기 자바스크립트에서는 주로 '+' 연산자가 문자열을 합치는 데 사용되었습니다. 하지만 ES6(ES2015)부터는 템플릿 리터럴이라는 깔끔한 방법이 생겼죠. '`' 문자로 문자열을 감싸고, `${}` 구문으로 변수를 삽입합니다. 문자열 연결을 할 때, '+' 연산자는 메모리와 성능에 영향을 더 미칠 수 있는 반면, 템플릿 리터럴은 내부적으로 최적화가 잘 되어있어 성능상의 이점이 있습니다. 끝으로, 템플릿 리터럴은 멀티 라인 문자열을 만드는 것도 간단하게 해줍니다.

## See Also (관련 자료)
- MDN Web Docs의 [템플릿 리터럴](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Template_literals)
- MDN Web Docs의 [문자열](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String)
