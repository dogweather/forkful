---
title:    "Javascript: 날짜를 문자열로 변환하기"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 것이 왜 중요한지를 이해할 수 있습니다. Javascript에서는 날짜를 다루는 여러 가지 방법이 있는데, 그 중에는 문자열로 변환하는 방법이 필요할 때가 있습니다. 날짜와 관련된 작업을 할 때, 문자열로 변환한 날짜를 사용하면 편리하고 쉽게 데이터를 다룰 수 있습니다. 

## 어떻게

날짜를 문자열로 바꾸는 방법은 여러 가지가 있지만, 가장 간단한 방법은 Date 객체의 `toDateString()` 메소드를 사용하는 것입니다. 예제 코드를 통해 어떻게 사용하는지 살펴보겠습니다.

```Javascript
let today = new Date(); // 현재 날짜를 가져옵니다.
let dateString = today.toDateString(); // 날짜를 문자열로 변환합니다.

console.log(dateString); // 예상 출력: Fri Aug 06 2021
```

위 코드에서는 `new Date()`를 통해 현재 날짜를 가져오고, `toDateString()` 메소드를 통해 문자열로 변환한 뒤, `console.log()`를 사용하여 출력합니다. 이처럼 간단하게 문자열로 날짜를 변환할 수 있습니다.

또 다른 방법으로는 `toLocaleDateString()` 메소드를 사용하는 것입니다. 이 메소드는 지역화된 날짜 형식으로 문자열을 반환해줍니다. 예제를 통해 살펴보겠습니다.

```Javascript
let today = new Date();
let options = { year: 'numeric', month: 'long', day: 'numeric' }; // 지역화 옵션 설정
let dateString = today.toLocaleDateString('en-US', options); // 지역화 옵션을 적용한 문자열로 변환합니다.

console.log(dateString) // 예상 출력: August 6, 2021
```

위 코드에서는 `toLocaleDateString()` 메소드를 사용하여 옵션 값을 설정한 뒤, `en-US`를 인자로 전달하여 미국식으로 날짜를 표현한 문자열을 반환받았습니다. `en-US` 대신 다른 나라나 언어의 옵션을 전달하면 해당 국가의 형식에 맞춰서 날짜가 표현됩니다.

## 깊이 파고들기

숫자로 표현되어 있는 날짜를 일반적으로 이해하기 어려우므로, 문자열로 변환하여 읽기 쉽게 만드는 것은 매우 중요합니다. 하지만 문자열로 날짜를 변환할 때, 주의해야 할 점이 있습니다. Javascript에서 날짜 객체는 timezone과 연관되어 있기 때문에, 변환된 문자열에도 timezone이 그대로 포함되어 있을 수 있습니다. 이를 방지하기 위해 `toUTCString()` 메소드를 사용하거나, `getTimezoneOffset()` 메소드를 사용하여 timezone을 따로 계산하는 방법을 고려할 수 있습니다.

## 참고

- [MDN Web Docs: Date.prototype.toDateString()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date/toDateString)
- [MDN Web Docs: Date.prototype.toLocaleDateString()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)
- [MDN Web Docs: Date.prototype.toUTCString()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date/toUTCString)
- [MDN Web Docs: Date.prototype.getTimezoneOffset()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date/getTimezoneOffset)