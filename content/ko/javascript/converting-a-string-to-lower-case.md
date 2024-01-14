---
title:                "Javascript: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 왜
문자열을 소문자로 변환하는 것에 관심을 가지는 이유는 여러 가지가 있을 수 있습니다. 예를 들어, 대소문자를 구분하지 않는 검색 기능을 구현하거나, 사용자로부터 입력 받은 문자열을 일관된 형식으로 처리하기 위해 사용할 수 있습니다.

## 하는 법
자바스크립트에서 문자열을 소문자로 변환하는 가장 간단한 방법은 `toLowerCase()` 메소드를 사용하는 것입니다. 이 메소드는 문자열 객체에 적용되며, 해당 문자열의 모든 문자를 소문자로 변환하여 새로운 문자열을 반환합니다.

예시:
```Javascript
let str = "JAVASCRIPT";
console.log(str.toLowerCase());
```
결과:
```Javascript
javascript
```
또 다른 방법으로는 `String.fromCharCode()` 메소드와 `charCodeAt()` 메소드를 조합하여 각 문자의 유니코드 값을 가져와 소문자로 변환하는 방법이 있습니다.

예시:
```Javascript
let str = "JAVASCRIPT";
let lowerCaseStr = "";
for (let i = 0; i < str.length; i++) {
  let charCode = str.charCodeAt(i);
  if (charCode >= 65 && charCode <= 90) { // A-Z의 유니코드 값 범위
    lowerCaseStr += String.fromCharCode(charCode + 32); // 대문자에서 32를 더해 소문자로 변환
  } else {
    lowerCaseStr += str[i];
  }
}
console.log(lowerCaseStr);
```
결과:
```Javascript
javascript
```

## 깊이있게 알아보기
위에서 소개한 두 번째 방법은 유니코드 값을 사용하여 문자를 소문자로 변환하는 방법입니다. 유니코드는 전 세계의 모든 문자를 표현하기 위한 표준 코드이며, 각 문자마다 고유한 코드값을 가지고 있습니다. 자바스크립트에서는 `String.fromCharCode()` 메소드를 이용하여 해당 유니코드 값을 문자로 변환할 수 있습니다.

또한 `toLowerCase()` 메소드로 문자열을 변환할 때, 해당 문자가 알파벳이 아닌 경우에는 변환하지 않고 그대로 둡니다. 따라서 적절한 유니코드 값을 찾는 것이 중요합니다.

## 또 다른 학습 자료
- [MDN Web Docs - toLowerCase()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [MDN Web Docs - charCodeAt()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/charCodeAt)
- [JavaScript에서 유니코드 다루기](https://velog.io/@jakeseo_me/JavaScript%EC%97%90%EC%84%9C-%EC%9C%A0%EB%8B%88%EC%BD%94%EB%93%9C-%EC%9E%91%EC%84%B1%ED%95%98%EA%B8%B0)