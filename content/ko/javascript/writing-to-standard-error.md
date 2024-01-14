---
title:    "Javascript: 표준 오류 쓰기"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

프로그래밍을 하는 데 있어서 디버깅은 항상 중요한 부분입니다. 특히 "standard error"라고 불리는 에러 출력을 다룰 때, 이를 적절히 다루는 것은 매우 중요합니다. 이 블로그 포스트에서는 "standard error"의 중요성과 이를 다루는 방법을 알아보겠습니다.

## 방법

자바스크립트에서 "standard error"를 다루는 가장 기본적인 방법은 `console.error()`를 사용하는 것입니다. 아래의 예시 코드를 참고해보세요.

```Javascript
console.error("에러 메시지");
```

위 코드를 실행하면 콘솔에 "에러 메시지"가 출력됩니다. 이를 통해 우리는 가장 기본적인 방법으로 "standard error"를 다룰 수 있다는 것을 알 수 있습니다.

## 깊게 파고들기

`console.error()` 외에도 자바스크립트에서는 `process.stderr.write()`를 통해 "standard error"를 다룰 수 있습니다. 이 메소드는 첫 번째 매개변수로 에러 메시지를 전달받고, 해당 메시지를 "standard error"에 출력합니다. 아래의 예시 코드를 참고해보세요.

```Javascript
process.stderr.write("에러 메시지");
```

위 코드를 실행하면 콘솔에 "에러 메시지"가 출력됩니다. 이 메소드는 `console.error()`보다 좀 더 특수한 경우에 사용될 수 있습니다. 따라서 개발자는 상황에 맞게 적절한 방법을 선택하여 "standard error"를 다루어야 합니다.

## 더 많은 정보

"standard error"를 다루는 것은 개발자에게 필수적인 기술 중 하나입니다. 따라서 자바스크립트에서 이를 제대로 다루고 익숙하게 사용하는 것은 중요합니다. 더 많은 정보는 아래의 링크들을 참고해보세요.

## 또 다른 참고 자료

- [MDN - console.error()](https://developer.mozilla.org/ko/docs/Web/API/Console/error)
- [Node.js - process.stderr](https://nodejs.org/api/process.html#process_process_stderr)
- [standard error로 디버깅하기](https://joshtronic.com/2016/02/29/debugging-with-stderr/) (영문)