---
title:                "TypeScript: 문자열 대문자 변경하기"
simple_title:         "문자열 대문자 변경하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 첫 번째 문자를 대문자로 변환하는 것이 유용한 경우는 다양합니다. 예를 들어, 데이터를 처리하거나 사용자의 이름을 출력할 때, 대문자로 통일된 형식이 필요한 경우가 있습니다. 이러한 상황에서 첫 글자를 자동으로 대문자로 변환하는 함수를 만드는 것은 매우 편리합니다.

## 방법

```TypeScript
function capitalizeString(str: string) {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalizeString("korean"));  // Korean
console.log(capitalizeString("typescript"));  // Typescript
console.log(capitalizeString("programming"));  // Programming
```

위의 코드는 문자열을 첫 번째 문자를 대문자로 변환하여 출력하는 함수를 보여줍니다. `str.charAt(0)`를 사용하여 첫 글자를 가져와서 `toUpperCase()` 메소드를 사용해 대문자로 변환하고, `str.slice(1)`를 사용하여 나머지 문자열을 잘라붙입니다.

## 딥 다이브

위의 코드에서 `capitalizeString` 함수의 인자로 넘겨준 문자열의 길이가 0이거나 1인 경우는 고려하지 않았습니다. 이를 방지하기 위해서는 문자열 길이를 체크하는 추가적인 로직을 구현해야 합니다. 또한, 한글 또는 다른 언어의 첫 글자가 대문자가 아닌 경우도 고려해야 합니다. 이를 해결하는 보다 robust한 함수를 만들기 위해서는 문자열 변환 과정에서 정규식을 사용하는 것이 도움이 될 수 있습니다.

## 참고

- [String.prototype.charAt()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [String.prototype.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [String.prototype.slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)