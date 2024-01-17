---
title:                "문자열 보간하기"
html_title:           "TypeScript: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열을 보간한다는 것은, 일종의 문자열 안에 변수 값을 삽입하는 것을 말합니다. 프로그래머들은 이를 자주 사용하는데, 이는 코드를 더 읽기 쉽고 유지보수하기 쉽게 만들기 때문입니다.

## 방법:

```TypeScript
let language = "TypeScript";
let text = `안녕하세요, 저는 ${language}를 사용하여 코딩하고 있습니다.`;
console.log(text);
```

결과:

```
안녕하세요, 저는 TypeScript를 사용하여 코딩하고 있습니다.
```

## 깊이 파고들기:

보간 문자열은 ES2015 스펙부터 도입되었는데, 이전에는 문자열 더하기 연산을 사용하여 같은 결과를 얻을 수 있었습니다. 그러나 이는 가독성이 떨어지고 번거로웠기 때문에 보간 문자열이 도입되었습니다. 다른 언어에서도 비슷한 기능이 지원되지만, TypeScript의 경우 보간 문자열을 더 엄격하게 검사하여 자바스크립트의 버그를 방지할 수 있습니다. 

## 더 알아보기:

- [MDN - 문자열 보간](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Template_literals)
- [TypeScript 공식 문서 - 보간 문자열](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html)