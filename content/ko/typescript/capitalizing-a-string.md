---
title:                "문자열 대문자로 변환하기"
date:                  2024-01-19
html_title:           "Arduino: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"

category:             "TypeScript"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열 대문자화는 작은 문자들을 모두 대문자로 바꾸는 처리 과정입니다. 프로그래머들이 이 작업을 해주는 이유는 주로 UI 표현의 일관성을 위해 혹은 사용자 입력 데이터를 표준화하는 데 있습니다.

## How to: (방법)
```typescript
function capitalizeString(input: string): string {
  return input.toUpperCase();
}

// 사용 예시
const originalString = "hello world";
const capitalized = capitalizeString(originalString);
console.log(capitalized);  // "HELLO WORLD"
```

## Deep Dive (심화 탐구)
문자열을 대문자로 바꾸는 방법은 컴퓨터 과학 초기부터 있어왔습니다. ASCII 테이블은 대-소문자 간의 명확한 숫자 차이를 가지고 있기 때문에 간단한 연산으로 대소문자를 전환할 수 있습니다. 자바스크립트에서는 `toUpperCase()`라는 내장 메소드를 사용하여 이 처리를 손쉽게 할 수 있으며 TypeScript 역시 자바스크립트의 이 메소드를 그대로 사용합니다. 다양한 대체 방법이 존재하지만, `toUpperCase()`는 가장 널리 사용되고 표준화된 방법입니다. 구현 세부사항에서 한 가지 주의할 점은 유니코드 문자가 포함된 문자열에 대문자화를 적용할 때 일부 특수 문자는 예상치 못한 결과를 초래할 수 있다는 점입니다. 따라서, 다국어 혹은 특수 기호를 처리할 때는 추가적인 논리가 필요할 때가 있습니다.

## See Also (참고 자료)
- [MDN toUpperCase() documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Unicode and JavaScript](https://flaviocopes.com/javascript-unicode/)
- [ASCII Table Reference](https://www.asciitable.com/)
