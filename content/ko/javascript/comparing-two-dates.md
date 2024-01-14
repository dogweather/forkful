---
title:    "Javascript: 날짜 비교하기"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## 왜

날짜를 비교하는 이유는 매우 다양합니다. 예를 들어, 기간을 비교하여 얼마나 오래 일을 하고 있는지 알 수 있습니다. 또는 유효기간이 만료되는 제품들을 업데이트해야 할 때도 날짜 비교가 필요합니다. 즉, 정확한 시간과 날짜를 비교하는 것은 매우 중요하며 많은 개발자들이 그러한 작업을 수행합니다.

## 어떻게

당신이 언어에서 날짜를 비교하는 방법에 대해 알고 싶다면, 다음 예시를 살펴보세요.

```Javascript
// 두 날짜를 생성합니다.
let firstDate = new Date('2020-01-01');
let secondDate = new Date('2020-02-01');

// compare 메소드를 사용하여 두 날짜를 비교합니다.
if (firstDate < secondDate) {
  console.log('첫 번째 시간이 더 빠릅니다!');
} else if (firstDate > secondDate) {
  console.log('두 번째 시간이 더 빠릅니다!');
} else {
  console.log('두 시간은 같습니다!');
}

// 결과:
// '첫 번째 시간이 더 빠릅니다!'
```

위 예시에서는 두 날짜를 생성하고, 이를 비교하여 결과를 출력하는 방법을 보여줍니다.

## Deep Dive

JavaScript에서 날짜를 비교하는 더 많은 방법이 있습니다. 예를 들어, `getTime()` 메소드를 사용하여 날짜를 밀리초로 변환한 다음, 이를 비교할 수도 있습니다. 또는 `getTimezoneOffset()` 메소드를 사용하여 서로 다른 타임존에서 날짜를 비교할 수도 있습니다. 이 외에도 `Date` 객체의 다른 메소드들을 사용하여 날짜를 비교하는 방법을 할 수 있습니다.

## 또 보기

* [Date 객체 - JavaScript | MDN](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
* [moment.js - Powerful JavaScript date library for parsing, validating, manipulating, and formatting dates.](https://momentjs.com/)