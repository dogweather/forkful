---
title:    "Javascript: 표준 에러로 쓰기"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## 왜
왜 누군가는 에러를 입력하기 위해 코딩하는지 궁금하십니까? 자바스크립트 프로그래밍에서 "표준 에러"는 디버깅 및 오류 찾기에 있어서 매우 유용한 도구입니다. 에러를 입력하면 프로그래밍 과정에서 발생하는 문제를 빠르게 파악하고 해결할 수 있습니다.

## 진행 방법
에러를 입력하는 방법은 간단합니다. 자바스크립트 코드 내에서 "console.error()" 함수를 사용하면 표준 에러로 메시지를 출력할 수 있습니다. 예를 들어, 아래 코드를 실행하면 "에러 발생!" 이라는 메시지가 표준 에러로 출력됩니다.

```Javascript
console.error("에러 발생!");
```

출력 결과는 다음과 같습니다.

```
> 에러 발생!
```

이렇게 간단한 코드만으로도 에러를 입력할 수 있으며, 이를 통해 프로그래밍에서 발생한 문제를 빠르게 파악할 수 있습니다.

## 심층 탐구
표준 에러는 여러 가지 방식으로 사용할 수 있습니다. 예를 들어, "console.error()" 함수를 사용하여 에러 메시지뿐만 아니라 변수의 값을도 출력할 수 있습니다. 또한 "try...catch" 문과 함께 사용하여 예외 처리를 할 수도 있습니다.

표준 에러는 프로그래밍에서 오류를 찾는 것뿐만 아니라, 프로그램의 실행 과정을 디버깅하는 데에도 매우 유용합니다. 따라서 프로그래밍을 하실 때에는 "console.error()" 함수를 적극적으로 활용하여 빠르고 신뢰성 있는 코드를 작성할 수 있도록 노력해보세요.

## 참조 자료
- [MDN web docs - console.error()](https://developer.mozilla.org/ko/docs/Web/API/Console/error)
- [W3Schools - console.error()](https://www.w3schools.com/jsref/met_console_error.asp)
- [오류 출력 함수의 사용법](https://devpact.com/186)
- [자바스크립트 디버깅을 위한 콘솔 활용하기](https://insertkim.com/javascript-dev-console/)

## 참고
[Taking Advantage of console.error() in JavaScript](https://medium.com/young-developer/taking-advantage-of-console-error-in-javascript-62c86dfb0e0a)