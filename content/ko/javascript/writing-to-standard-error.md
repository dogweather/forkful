---
title:                "Javascript: 표준 오류 쓰기"
simple_title:         "표준 오류 쓰기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜
우리는 모두 프로그래밍을 할 때 버그를 피할 수 없습니다. 그러나 그들을 찾고 해결하기 위해서는 프로그램에서 발생하는 모든 에러를 신속하게 파악할 수 있어야합니다. 여기서 하나의 도구로 여러분의 생각을 예외적인 바이트로 출력하고 자세한 원인을 찾는 데 깊게 파있는 방법이 바로 표준 에러 출력입니다.

## 글을 쓰는 방법
표준 에러를 생성하고 기록하는 작업은 간단한 방법입니다. 먼저, `console.error()` 메서드를 사용하여 원하는 메시지를 출력 할 수 있습니다. 그리고 그 밑에는 추가적인 정보도 쓸 수 있습니다. 아래의 예시 코드 에서는 이 메서드가 어떻게 작동하는지 보여줍니다.

```javascript
const age = undefined;
console.error("나이는 정의되지 않았습니다.");
console.error(`나이는 ${age}입니다.`);
```
출력:
```
나이는 정의되지 않았습니다.
나이는 undefined입니다.
```

## 깊게 파보기
자, 이제 `console.error()` 메서드의 작동 방식을 좀 더 자세히 살펴보겠습니다. 이 메서드는 TypeScript와 같은 자바스크립트의 상위 언어이기도 한 `console.log()` 메서드와 매우 유사하지만, 약간 다른 목적을 가지고 있습니다. `console.log()`는 간단한 로그 메시지를 출력하는 데 사용되지만, `console.error()`는 에러 메시지를 출력하고 콘솔 창에 눈에 잘 띄게 표시되도록 합니다. 또한, `console.error()`는 스택 추적 정보를 출력해주므로 원인을 찾는 데 도움이 됩니다. 이 메서드는 개발을 할 때 빠른 디버깅에 매우 유용하게 쓰일 수 있습니다.

## 이것도 볼만합니다
- [console.error() - MDN 웹 문서](https://developer.mozilla.org/ko/docs/Web/API/console/error)
- [Error 객체 - MDN 웹 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Error)
- [처음 사용하는 자바스크립트 콘솔에 대해 알아보기 - MDN 웹 문서](https://developer.mozilla.org/ko/docs/Learn/JavaScript/First_steps/What_you_Will_need)