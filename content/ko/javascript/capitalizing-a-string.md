---
title:                "문자열 대문자로 변환하기"
html_title:           "Arduino: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열 대문자화는 문자열의 첫 글자를 대문자로 바꾸는 것입니다. 프로그래머들은 사용자 인터페이스를 일관되게 보이게 하고, 데이터를 정규화하기 위해 이 작업을 종종 수행합니다.

## How to: (어떻게 하나요?)
```javascript
// 간단한 대문자화 함수
function capitalize(str) {
  if(str && typeof str === 'string') {
    return str.charAt(0).toUpperCase() + str.slice(1);
  }
  return str;
}

// 사용 예시
console.log(capitalize('hello')); // 'Hello'
console.log(capitalize('')); // ''
console.log(capitalize(null)); // null
```

## Deep Dive (심층 분석)
대소문자 변환은 프로그래밍 초기부터 있어왔습니다. 문자열의 가독성을 높이는 간단하지만 강력한 방법이죠. JavaScript에서는 `.toUpperCase()`와 `.toLowerCase()` 메서드를 내장하여 각 문자의 대문자와 소문자 변환이 가능합니다.

대문자로 만들 때 주의할 점은 로케일(Locales)입니다. 특정 언어에서는 특수한 대문자 규칙을 가지고 있습니다. 이를 위해 `toLocaleUpperCase()` 메소드를 사용할 수 있습니다.

```javascript
// 'i'의 터키어 대문자 변환 예시
console.log('i'.toLocaleUpperCase('tr-TR')); // 'İ'
```

대문자화는 프로그래밍 언어가 진화하면서 여러 방법으로 구현할 수 있게 되었습니다. 이러한 방법 중 하나는 첫 글자만 대문자로 만드는 것입니다. `charAt()`, `slice()` 메서드를 사용하여 이를 쉽게 구현할 수 있습니다.

자바스크립트 프레임워크나 라이브러리에서도 유사한 기능을 제공합니다. 예를 들어, Lodash 라이브러리는 `_.capitalize` 함수를 제공합니다.

## See Also (참고 자료)
- MDN Web Docs: String.prototype.toUpperCase() - [MDN toUpperCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- MDN Web Docs: String.prototype.toLocaleUpperCase() - [MDN toLocaleUpperCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleUpperCase)
- Lodash capitalize documentation - [Lodash capitalize](https://lodash.com/docs/#capitalize)