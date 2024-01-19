---
title:                "미래 혹은 과거의 날짜 계산하기"
html_title:           "TypeScript: 미래 혹은 과거의 날짜 계산하기"
simple_title:         "미래 혹은 과거의 날짜 계산하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Dates 계산하기: 미래 혹은 과거 시간 계산하는 이유와 방법

## 무엇이고 왜?

날짜 계산은 미래나 과거의 특정 일자를 추정하는 것을 의미합니다. 이는 사건이 일어난 시간을 추적하거나, 특정 기간이 경과한 후의 날짜를 예측하는 등의 작업을 위해 프로그래머들이 사용합니다.

## 어떻게 하나:

타입스크립트에서는 `Date` 객체를 사용하여 날짜를 계산합니다. 아래의 코드를 참고해 보세요:

```TypeScript
let now: Date = new Date(); // 현재 시간
console.log("현재: " + now);

let future: Date = new Date();
future.setDate(now.getDate() + 7); // 7일 후
console.log("미래: " + future);

let past: Date = new Date();
past.setDate(now.getDate() - 7); // 7일 전
console.log("과거: " + past);
```

이 코드의 출력결과는 다음과 같습니다:

```Output
현재: Sat Sep 11 2021 13:00:00 GMT+0900 (Korea Standard Time)
미래: Sat Sep 18 2021 13:00:00 GMT+0900 (Korea Standard Time)
과거: Sat Sep 04 2021 13:00:00 GMT+0900 (Korea Standard Time)
```

## 깊이 있게 알아보기

### 역사적 맥락

컴퓨터가 등장한 초기 시점에는 날짜와 시간 계산은 어렵고 복잡한 작업 중 하나였습니다. 하지만 이제는 언어 자체에서 원하는 날짜를 계산하는 기능을 제공하고 있어 개발자들이 보다 쉽게 이를 처리할 수 있게 되었습니다.

### 대체 방법

자바스크립트 및 타입스크립트에서 현재 및 미래의 날짜를 계산하는 방법 중 하나는 `Date` 객체를 사용하는 것입니다. 그 외에도 `moment.js` 등의 라이브러리를 사용하면 확장된 기능을 보다 쉽게 사용할 수 있습니다.

### 실행 정보

위에서 제시한 방식은 간단하지만 주의할 점이 있습니다. `setDate`는 월을 넘어가는 경우에도 정상 작동하지만, 이는 월을 기준으로 계산을 하고 싶을 때 문제가 될 수 있습니다. 이럴 때는 `setMonth` 함수를 사용해야 할 것입니다.

## 참고하기

프로그래밍에서 다루는 날짜와 시간에 대해 더 알아보려면 아래의 링크를 참조하세요:

- [MDN Web Docs: Date](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [Day.js](https://day.js.org/)
- [Date-fns](https://date-fns.org/)