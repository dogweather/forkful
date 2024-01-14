---
title:                "TypeScript: 정규 표현식 사용하기"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜 정규 표현식을 사용하는가?
정규 표현식은 문자열을 다루는 데 매우 유용합니다. 문자열에서 특정한 규칙을 찾고 추출하거나 대체하는 등 다양한 작업을 할 수 있습니다. 또한 정규 표현식은 문자열 검색에 대한 패턴을 정의하는 데 도움이 됩니다. 그렇기 때문에 많은 프로그래머들이 정규 표현식을 사용하여 문자열을 다루고 있습니다.

## 정규 표현식 사용 방법
다음은 TypeScript로 정규 표현식을 사용하는 간단한 예제입니다. 먼저, 문자열 "Hello World"에서 "Hello"를 찾아 출력하는 코드를 살펴보겠습니다.
```TypeScript
const str = "Hello World";
const regex = /Hello/; //정규 표현식을 변수에 할당합니다.
console.log(str.match(regex)); // ['Hello'] 출력
```

정규 표현식은 다양한 패턴을 확인하여 문자열을 찾는데 사용될 수 있습니다. 예를 들어, 다음 코드는 이메일 주소의 형식이 올바른지 여부를 확인하는 패턴을 정의하고, 입력된 문자열이 패턴에 맞는지 확인하는 예제입니다.
```TypeScript
const emailInput = "example@domain.com";
const emailRegex = /^\w+@[a-zA-Z_]+?\.[a-zA-Z]{2,3}$/; // 이메일 형식을 정의하는 정규 표현식
console.log(emailInput.match(emailRegex)); // ['example@domain', 'com'] 출력
```

## 정규 표현식 깊이 알아보기
정규 표현식을 좀 더 자세히 알아보기 위해 다음과 같은 패턴 매칭에 대한 개념을 이해해야 합니다.

- `^` : 문자열의 시작 부분을 찾습니다.
- `$` : 문자열의 끝 부분을 찾습니다.
- `.` : 어떤 문자 하나와 일치합니다.
- `*` : 앞의 패턴이 0번 이상 반복되는 부분을 찾습니다.
- `+` : 앞의 패턴이 1번 이상 반복되는 부분을 찾습니다.
- `[ ]` : 대괄호 안의 문자 중 하나와 일치합니다.
- `\` : 이스케이프 문자로 다음 문자를 문자 그대로 해석합니다.

더 자세한 패턴 매칭 규칙은 [정규표현식 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)에서 확인할 수 있습니다.

## 관련 링크
- [TypeScript 공식문서](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [정규표현식 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)
- [정규표현식 연습 사이트](https://regexr.com/)
- [정규표현식 적용 예제](https://code.tutsplus.com/tutorials/8-regular-expressions-you-should-know--net-6149)