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

## 무엇 & 왜?
패턴과 일치하는 문자를 삭제하는 것은 프로그래머가 자주 수행하는 작업 중 하나입니다. 예를 들어, 우리는 주어진 문자열에서 숫자만을 남기려고 할 수 있습니다. 이를 통해 우리는 쉽게 숫자가 아닌 문자를 제거할 수 있습니다.

## 방법:
```Javascript
// 예시 1: "abc123def" 문자열에서 숫자만 제거하기
const string = "abc123def"
const result = string.replace(/[0-9]/g, "")
console.log(result) // 결과: "abcdef"

// 예시 2: "Apple, Orange, Banana" 문자열에서 쉼표(,) 제거하기
const string = "Apple, Orange, Banana"
const result = string.replace(/,/g, "")
console.log(result) // 결과: "Apple Orange Banana"
```

## 깊게 알아보기:
패턴과 일치하는 문자를 삭제하는 방법은 다양한 옵션을 제공합니다. 예를 들어, 정규식을 사용하는 대신 String의 split() 메서드를 사용하여 문자열을 배열로 변환하고 필요한 요소를 제외하는 방법도 있습니다. 또한 일부 프레임워크는 문자열의 특정 부분을 삭제하는 방법을 제공하기도 합니다.

또한, 해당 작업은 과거에는 수동으로 수행되었지만, 지금은 컴퓨터를 이용하여 코드를 작성하여 자동화할 수 있습니다. 이를 통해 작업의 효율성을 높일 수 있습니다.

## 관련 자료:
- [String.prototype.replace()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [String.prototype.split()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/split)
- [정규식 테스트 사이트](https://regexr.com/)