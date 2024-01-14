---
title:                "TypeScript: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

"## 왜"

컴퓨터 프로그래밍에서 우리는 종종 날짜를 비교해야 합니다. 날짜를 비교하는 이유는 우리가 특정 기간 안에 어떤 일이 발생했는지, 또는 두 날짜 사이의 차이를 계산하는데 필요하기 때문입니다.

## 어떻게

TypeScript에서 두 날짜를 비교하는 방법은 다소 복잡할 수 있지만, 이 글에서는 간단한 예제와 함께 쉽게 이해할 수 있도록 설명하겠습니다.

### 두 날짜 비교하기

두 날짜를 비교하기 위해서는 우선 Date 객체를 생성해야 합니다. 그리고 Date 객체에서 제공하는 메서드를 이용하여 비교할 수 있습니다.

```TypeScript
// 2021년 8월 10일과 2021년 8월 12일 비교하기
const date1: Date = new Date(2021, 7, 10); // 월은 0부터 시작하므로, 8월을 나타내려면 7을 입력해야 함
const date2: Date = new Date(2021, 7, 12);

// 두 날짜가 같은지 비교하기
console.log(date1.getTime() === date2.getTime()); // false

// 첫 번째 날짜가 두 번째 날짜보다 이후인지 비교하기
console.log(date1.getTime() > date2.getTime()); // false

// 두 번째 날짜가 첫 번째 날짜보다 이후인지 비교하기
console.log(date2.getTime() > date1.getTime()); // true
```

### 날짜 차이 계산하기

두 날짜 사이의 차이를 계산하기 위해서는 먼저 두 날짜 사이의 밀리초 차이를 계산해야 합니다. 그리고 이 값을 원하는 단위(일, 시간, 분, 초 등)로 변환하면 됩니다.

```TypeScript
// 2021년 8월 10일과 2021년 8월 12일 비교하기
const date1: Date = new Date(2021, 7, 10); // 월은 0부터 시작하므로, 8월을 나타내려면 7을 입력해야 함
const date2: Date = new Date(2021, 7, 12);

// 두 날짜 사이에 몇 일 차이가 있는지 계산하기
const dayDiff: number = Math.round(Math.abs(date2.getTime() - date1.getTime()) / (1000 * 3600 * 24));

console.log(`${date2.getFullYear()}년 ${date2.getMonth() + 1}월 ${date2.getDate()}일과 ${date1.getFullYear()}년 ${date1.getMonth() + 1}월 ${date1.getDate()}일 사이의 차이는 ${dayDiff}일 입니다.`);
// 출력 결과: 2021년 8월 12일과 2021년 8월 10일 사이의 차이는 2일 입니다.
```

## 조금 더 깊게 알아보기

TypeScript에서 날짜를 다루는 방법은 JavaScript와 거의 동일합니다. 따라서 JavaScript에서 제공하는 Date 객체의 메서드와 프로퍼티를 사용할 수 있습니다. 더 자세한 내용은 [MDN 웹 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)를 참고해주세요.

## 관련 자료

- [TypeScript 공식 문서](https://www.typescriptlang.org/docs/handbook/declaration-merging.html)
- [MDN 웹 문서](https://developer.mozilla.org/ko/docs