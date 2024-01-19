---
title:                "두 날짜 비교하기"
html_title:           "C#: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

날짜를 비교한다는 것은 두 개의 날짜를 동시에 점검하고, 둘 중 어느 것이 더 이른지, 혹은 나중인지를 판단하는 과정입니다. 프로그래머들은 이를 통해 이벤트의 순서, 기간의 확인 등 다양한 로직 구현시에 필요한 시간적인 정보를 얻습니다.

## 방법:

이제 어떻게 자바스크립트에서 날짜를 비교할 수 있는지 살펴봅니다.

```Javascript
let date1 = new Date('2021-12-01');
let date2 = new Date('2022-01-01');

if(date1 > date2) {
  console.log('date1 is later');
} else if(date1 < date2) {
  console.log('date1 is earlier');
} else {
  console.log('dates are equal');
}
```

위 코드를 실행하면 date1이 date2보다 이르기 때문에 'date1 is earlier'라는 메시지가 출력됩니다.

## 깊은 이해를 위한:

- **역사적 맥락**: 자바스크립트의 초기 버전에서는 날짜비교 방식이 현재와 다소 다르게 기능했습니다. 그러나 ECMAScript의 새 버전에서는 `Date` 객체를 사용하여 두 날짜를 정확하고 효과적으로 비교할 수 있습니다.

- **대안들**: 비교연산자 외에도 `getTime()` 함수를 이용하여 밀리세컨드를 반환받아 두 날짜를 비교하는 방법도 있습니다.

```Javascript
if(date1.getTime() == date2.getTime()) {
    console.log('dates are equal');
}
```

- **구현세부사항**: 자바스크립트에서 비교 연산자를 사용할 때, 자동으로 `getTime()` 함수가 호출되어 두 날짜의 밀리세컨드를 비교합니다. 그렇기 때문에 위의 두 방법은 기본적으로 동일합니다.

## 참고하고 싶은:

- [Mozilla Developer Network의 Date 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [자바스크립트에서 날짜를 비교하는 다른 방법에 대한 Stack Overflow 질문](https://stackoverflow.com/questions/492994/compare-two-dates-with-javascript)