---
title:    "Javascript: 패턴과 일치하는 문자 삭제하기"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## 왜

왜 문자 패턴과 일치하는 문자를 삭제하는 것에 참여해야 할지에 대해 알아보겠습니다. 대부분의 경우, 이는 문자열 정제 과정에서 필요할 수 있습니다. 예를 들어, 사용자가 입력한 데이터를 정제하거나 특정 패턴에 맞지 않는 문자들을 제거할 때 사용할 수 있습니다.

## 방법

자바스크립트를 사용하여 문자 패턴과 일치하는 문자를 삭제하는 방법을 살펴보겠습니다. 우선, `replace()` 메소드를 사용하여 패턴에 맞는 문자를 빈 문자열로 대체합니다. 그런 다음, 결과를 출력하거나 새 변수에 할당하면 됩니다.

```Javascript
let string = "Hello World!";
let newString = string.replace(/[aeiou]/g, '');
console.log(newString); // Output: Hll Wrld!
```

만약, 삭제하고 싶은 문자가 여러 개의 패턴에 일치하는 경우는 어떻게 해야 할까요? 이 경우, `|`를 사용하여 여러 패턴을 나란히 나열하면 됩니다.

```Javascript
let string = "1, 2, 3, 4";
let newString = string.replace(/1|3|5/g, '');
console.log(newString); // Output: , 2, , 4
```

## 깊이 파헤치기

삭제하고 싶은 문자를 조금 더 세부적으로 설정하고 싶은 경우는 어떻게 해야 할까요? 이 경우, 정규표현식을 사용하여 더 많은 옵션을 설정할 수 있습니다. 예를 들어, `/\s|[,\.]/g`와 같이 사용하면 공백 및 `,`와 `.`을 모두 삭제할 수 있습니다. 또한, `[]` 내에 문자를 나열하여 원하는 문자만 삭제할 수도 있습니다.

## 또 다른 정보

마지막으로, 아래의 링크들에서 더 많은 정보를 찾아볼 수 있습니다.

- [String.prototype.replace()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [정규표현식 - MDN 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)
- [정규표현식 연습 - RegexOne](https://regexone.com/)