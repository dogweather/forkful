---
title:    "Javascript: 디버그 출력하는 방법"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜 디버그 출력을 사용하는가?

프로그래밍을 하다보면 어떤 부분에서 코드가 원하는 대로 작동하지 않는 경우가 있습니다. 이 때 디버그 출력을 사용하면 코드가 어떻게 작동되고 있는지 쉽게 파악할 수 있습니다. 디버그 출력은 오류를 수정하는 데 매우 유용한 도구이며 프로그램의 작동 방식을 이해하는 데 도움이 됩니다.

## 디버그 출력하는 방법

디버그 출력은 자바스크립트에서 `console.log()` 함수를 사용하여 출력할 수 있습니다. 아래의 예시 코드에서는 변수 `x`의 값을 출력하고 있습니다.

```Javascript
let x = 10;
console.log(x);
```

실행 결과는 다음과 같이 콘솔에 출력됩니다.

```
10
```

이와 같이 `console.log()`를 사용하여 변수, 함수 등의 값을 쉽게 확인할 수 있습니다. 또한, `console.log()`에 여러 값을 전달하는 경우 각 값들은 쉼표로 구분하여 출력됩니다.

```Javascript
let name = "John";
let age = 30;
console.log(name, age);
```

실행 결과는 다음과 같습니다.

```
John 30
```

## 디버그 출력에 대해 더 알아보기

디버그 출력은 `console.log()` 이외에도 다양한 방법으로 사용할 수 있습니다. `console.dir()` 함수는 객체의 프로퍼티 값을 출력하며 `console.trace()` 함수는 코드의 실행 경로를 출력합니다. 또한, `console.assert()` 함수를 사용하여 조건식을 통해 에러가 발생한 경우 메시지를 출력할 수도 있습니다.

디버그 출력을 사용할 때 주의할 점은 너무 많이 출력하면 오히려 코드를 파악하기 어려워질 수 있다는 것입니다. 따라서, 필요한 부분에서만 적절히 디버그 출력을 사용하는 것이 좋습니다.

## 관련 자료

- [MDN Web Docs: 디버깅](https://developer.mozilla.org/ko/docs/Tools/Debugger)
- [JavaScript Debugging for Beginners](https://www.w3schools.com/js/js_debugging.asp)
- [Debugging JavaScript Code with console.log()](https://blog.bitsrc.io/debugging-javascript-code-with-console-log-61123985eec5)