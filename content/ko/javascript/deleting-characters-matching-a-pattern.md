---
title:                "패턴과 일치하는 문자 삭제하기"
html_title:           "Javascript: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

캐릭터 매칭 패턴을 지우는 것에 관심이 있을 수 있는 이유는 문자열 데이터를 정리하거나 선택적으로 특정 문자열을 삭제하기 위해서입니다.

## 방법

예를 들어, 문자열 내에서 특정 단어를 삭제하고 싶다면 다음과 같이 할 수 있습니다.

```Javascript
let str = "Hello world, my name is John.";
let updatedStr = str.replace("John", "");
console.log(updatedStr);
// 결과: "Hello world, my name is."
```
위 예시에서는 `replace()` 함수를 사용하여 "John"이라는 단어를 빈 문자열로 대체하여 원하는 문자열을 삭제했습니다.

또는 정규식을 사용하여 패턴을 지정해 삭제할 수도 있습니다. 예를 들어, 다음 예시에서는 숫자를 모두 삭제하는 코드를 작성해보겠습니다.

```Javascript
let str = "I have 3 apples and 5 bananas.";
let updatedStr = str.replace(/\d+/g, "");
console.log(updatedStr);
// 결과: "I have apples and bananas."
```
위 예시에서는 `replace()` 함수의 첫 번째 매개변수에 정규식 `/d+/g`를 사용하였습니다. 이 정규식은 문자열 내에서 모든 숫자를 찾아 삭제하도록 지정한 것입니다.

## 깊이 들어가기

`replace()` 함수는 문자열 내에서 일치하는 첫 번째 패턴만을 삭제한다는 것에 주의해야 합니다. 만약 모든 패턴을 삭제하고 싶다면 `replace()` 함수의 두 번째 매개변수로 `g`라는 플래그 값을 설정해야 합니다. 또한, 정규식에 대한 이해가 부족하다면 패턴을 작성하는데 어려움이 있을 수 있습니다. 이 경우 인터넷에서 정규식 패턴을 찾아 사용하는 것도 좋은 방법이 될 수 있습니다. 

## 참고자료

- [MDN replace() 함수](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [정규식 패턴 빌더](https://regexr.com/)