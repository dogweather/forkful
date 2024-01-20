---
title:                "문자열을 소문자로 변환하기"
html_title:           "Bash: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇이며 왜합니까? (What & Why?)

문자열을 소문자로 변환하는 것은 정확히 그대로의 의미입니다: 대문자가 있는 문자열을 받아 모든 문자를 소문자로 바꾸는 것입니다. 이는 대소문자 구분 없이 문자열을 비교하거나 정렬할 때 매우 유용합니다. 

## 어떻게 할까? (How to)

JavaScript에서는 이를 위한 기본 메서드 `toLowerCase()`를 제공합니다. 

```Javascript
var myString = "Hello, World!";
console.log(myString.toLowerCase());
```

위 코드의 출력값

```Javascript
"hello, world!"
```

## 깊이 파보기 (Deep Dive)

1. **역사적 맥락** : JavaScript의 `toLowerCase()` 메서드는 처음 JavaScript가 나타났을 때부터 있었던 기능 중 하나입니다. 이 메서드는 문자열에 대소문자 구분 없이 조작을 하는 경우에 큰 힘이 됩니다.
2. **대체 방법** : 이외에도 대소문자 구분없이 비교를 해야하는 경우 `localeCompare()` 함수를 사용할 수 있습니다. 그러나 `toLowerCase()`를 사용하면 더 간결하게 코드를 작성할 수 있습니다.
3. **구현 세부 사항** : `toLowerCase()` 메서드는 문자열의 모든 대문자를 해당하는 소문자로 변환합니다. 이 변환은 호환 가능한 표준 유니코드로 수행됩니다.

## 참고 자료 (See Also)

- MDN Web Docs의 `toLowerCase()` 설명서는 여기서 확인하실 수 있습니다. [MDN toLowerCase](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- 대소문자 변환에 대한 모든 상세한 유니코드 매핑 정보는 [여기](https://www.unicode.org/Public/UNIDATA/UnicodeData.txt)서 찾아볼 수 있습니다.