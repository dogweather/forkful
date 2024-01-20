---
title:                "문자열 보간하기"
html_title:           "Clojure: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇인가요 & 왜요?

문자열 보간(Interpolation)이란, 변수가 선언되고 할당된 후에 이 변수를 문자열 안에 직접 넣는 것을 말합니다. 그래서 복잡한 문자열을 간편하게 만들 수 있어서 프로그래머들이 주로 사용합니다.

## 어떻게 사용하나요:

```TypeScript
let name: string = "John";
console.log(`Hello, ${name}!`);
```
위의 코드를 실행하면 다음의 출력 결과를 볼 수 있습니다:
```
Hello, John!
```

## 깊은 이해를 위해:

1. **역사적 맥락**: 이전에는 JavaScript에서 문자열 연결을 위해 '+' 연산자를 사용했지만, 이 방법은 복잡하고 가독성이 떨어집니다. TypeScript의 문자열 보간 기능은 이를 개선하였습니다.

2. **대안자**: 문자열 보간 이외에도, `concat()` 메서드, `+` 연산자 등을 이용해 문자열을 결합할 수 있습니다. 그러나 보간법이 이들보다 간편하고 가독성이 좋습니다.

3. **구현상세**: TypeScript에서 문자열 보간은 변수값이 문자열 안에 포함되도록 템플릿 리터럴(``)을 사용합니다. `${}` 함수는 이 함수 안에 있는 변수들을 대신하여 그 값들을 출력합니다.

## 참고 자료:

- MDN Web Docs, [Template literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)