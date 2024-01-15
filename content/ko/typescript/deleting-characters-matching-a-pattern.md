---
title:                "패턴과 일치하는 문자 삭제하기"
html_title:           "TypeScript: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

문자 일치 패턴을 삭제하는 것에 참여할 이유는 무엇일까요? 일치하는 문자를 삭제하는 것은 특정 텍스트에서 불필요한 정보를 제거하여 작업을 쉽게 하거나, 원하는 결과를 얻기 위해서 일반적으로 사용됩니다.

## 어떻게

일치하는 문자를 삭제하는 방법을 알아보겠습니다. 먼저, `replace()` 메소드를 사용하여 문자열에서 특정 패턴을 삭제할 수 있습니다.

```typescript
let str = "Hello, world!";
let result = str.replace(/[aeiou]/gi, "");
console.log(result); // Hll, wrld!
```

위의 예제에서는 `replace()` 메소드를 사용하여 `str` 문자열에서 모음을 삭제하였습니다. ".replace()의 첫 번째 매개변수로 전달된 `[aeiou]`는 패턴을 정의하는 정규식입니다. 그리고 두번째 매개변수로 빈 문자열을 전달하여 모음을 삭제하도록 지정하였습니다. 코드 실행 결과, 모음이 삭제된 문자열인 `Hll, wrld!`가 출력됩니다.

## 깊이 파고들기

문자 일치 패턴을 삭제하는 것은 일상적으로 사용되는 작업입니다. 이 작업을 수행하는 다양한 방법이 있지만, 일관된 결과를 얻기 위해서는 정규식을 적절하게 활용하는 것이 중요합니다. 정규식을 제대로 이해하고 활용한다면, 더욱 다양한 작업을 수행할 수 있으며 보다 유연하게 문자열을 다룰 수 있습니다.

## 관련 자료

- [MDN Web Docs: replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Understanding Regular Expressions in JavaScript](https://code.tutsplus.com/tutorials/understanding-regular-expressions-in-javascript--net-25486) (영문)
- [Visual Studio Code: Regular Expressions](https://code.visualstudio.com/docs/editor/codebasics#_regular-expressions) (영문)