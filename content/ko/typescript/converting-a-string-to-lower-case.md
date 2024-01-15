---
title:                "문자열 소문자로 변환하기"
html_title:           "TypeScript: 문자열 소문자로 변환하기"
simple_title:         "문자열 소문자로 변환하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 것에 참여할 이유는 다양합니다. 우선, 대소문자를 구분하지 않는 곳에서 특정 문자열을 검색하고 비교할 때 유용합니다. 또한, 사용자 입력을 처리하거나 문자열 데이터를 정리할 때도 매우 유용합니다.

## 사용 방법

```TypeScript
// 문자열을 소문자로 변환하는 방법
const str = "Hello World";
const lowerStr = str.toLowerCase();

console.log(lowerStr); // output: hello world
```

## 깊게 들어가기

문자열을 소문자로 변환하기 위해 `toLowerCase()` 메서드를 사용합니다. 이 메서드는 원본 문자열을 변경하지 않고 새로운 소문자 문자열을 반환합니다. 또한, 이 메서드를 호출할 때 인자로 패턴을 전달할 수도 있습니다. 이 패턴은 선택적으로 대응되는 대상 문자열만 소문자로 변환합니다. 

예를 들어, `toLowerCase()` 메서드를 사용하여 `Hello World` 문자열을 소문자로 변환하면 `hello world`가 출력되지만, `o` 패턴을 추가하여 `toLowerCase('o')`로 호출하면 `HellO WOrld`가 출력됩니다.

## See Also

- [String.prototype.toLowerCase() - MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [JavaScript Strings - W3Schools](https://www.w3schools.com/js/js_strings.asp)
- [TypeScript Official Documentation](https://www.typescriptlang.org/docs/)