---
title:                "Javascript: 문자열 대문자화하기"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 첫 글자를 대문자로 바꾸는 것이 왜 중요한지 궁금하신가요? 대문자로 된 문자열은 더 강조할 수 있고, 가독성이 좋아져 다른 프로그래머들과의 협업이나 자신의 코드를 유지 관리할 때 유용합니다.

## 어떻게

자바스크립트에서 문자열의 첫 글자를 대문자로 바꾸는 방법은 간단합니다. `charAt()` 메소드를 사용하여 문자열의 첫 번째 글자를 가져온 후 `toUpperCase()` 메소드를 사용하여 대문자로 변환하면 됩니다. 아래 예시 코드를 참고해보세요.

```Javascript
let str = "hello world";
let firstLetter = str.charAt(0).toUpperCase();
console.log(firstLetter); // output: H
```

위 예시 코드에서 `str.charAt(0)`은 문자열 "hello world"의 첫 번째 글자 "h"를 가져오고, `.toUpperCase()` 메소드는 해당 글자를 대문자로 변환합니다. 이렇게 문자열의 첫 번째 글자를 대문자로 변환할 수 있습니다.

## 깊이 파고들기

문자열의 첫 번째 글자를 대문자로 바꾸는 방법은 여러 가지가 있습니다. 위 예시 코드에서는 `charAt()` 메소드와 `toUpperCase()` 메소드를 사용했지만, `substring()` 메소드와 `replace()` 메소드를 사용하여도 첫 번째 글자를 대문자로 변환할 수 있습니다. 각각의 메소드를 사용하는 방법과 차이점을 깊이 파고들어보면 더 많은 것을 배울 수 있을 것입니다.

## 또 보기

- [자바스크립트 String 메소드](https://www.w3schools.com/jsref/jsref_obj_string.asp)
- [String.charAt() 메소드](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [String.toUpperCase() 메소드](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)