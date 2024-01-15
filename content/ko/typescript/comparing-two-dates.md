---
title:                "두 날짜 비교하기"
html_title:           "TypeScript: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

ㅇㅇ
"## 왜"

두 날짜를 비교하는 것이 왜 중요한지 궁금하셨나요? 비교를 통해 날짜의 순서를 파악하고, 날짜 간의 차이를 계산할 수 있습니다. 이를 통해 시간과 일정을 관리하는 데에 도움을 받을 수 있습니다.

"## 사용 방법"

\```TypeScript
// 두 가지 날짜 선언
const date1: Date = new Date("2021-01-31");
const date2: Date = new Date("2021-05-01");

// 날짜 비교
console.log(date1 < date2); // true
console.log(date1 > date2); // false
console.log(date1 === date2); // false
console.log(date1.getTime() === date2.getTime()); // true, 밀리초 단위로 비교

// 날짜 간의 차이 계산
const diffInDays: number = (date2.getTime() - date1.getTime()) / (1000 * 3600 * 24); // 일 단위로 변환
console.log(diffInDays); // 90
\```

위의 코드를 통해 Date 객체를 사용하는 방법과 여러 비교 연산자를 활용하여 날짜를 비교하는 방법을 알아볼 수 있습니다. 또한 getTime() 메소드를 통해 날짜 간의 차이를 계산할 수 있습니다.

"## 더 깊게"

Date 객체는 날짜와 시간을 나타내기 위해 사용되며, 값을 나타내는 시간대, 지역 등 여러 옵션이 존재합니다. 따라서 날짜를 비교할 때에도 이러한 옵션들을 고려해야 합니다. 또한 Date 객체뿐만 아니라 moment.js와 같은 라이브러리를 활용하여 더 다양한 날짜 관련 기능들을 사용할 수 있습니다.

"## 관련 자료"

- [MDN web docs - Date](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [TypeScript Official Site](https://www.typescriptlang.org/)