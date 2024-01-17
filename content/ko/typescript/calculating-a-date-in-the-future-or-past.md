---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "TypeScript: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
날짜를 미래나 과거로 계산하는 것은 특정 날짜를 기준으로 일정한 기간을 더하거나 빼는 것을 말합니다. 이는 프로그래머들이 다양한 작업에서 날짜와 시간을 처리하기 위해 자주 사용되기 때문에 중요합니다.

## 사용 방법:
```TypeScript
// 현재 날짜를 구하는 함수
const currentDate = new Date();

// 5일 뒤의 날짜를 계산하는 함수
const futureDate = new Date();
futureDate.setDate(currentDate.getDate() + 5);

// 계산 결과 출력
console.log(`현재 날짜: ${currentDate}`);
console.log(`5일 뒤의 날짜: ${futureDate}`);
```

## 깊이 들어가기:
1. 날짜를 계산하는 방법은 개발자에 따라 다양하게 다릅니다. 예를 들어서 TypeScript에서는 JavaScript Date 객체의 메소드를 사용하거나, Moment.js와 같은 외부 라이브러리를 사용할 수 있습니다. 
2. 날짜를 계산하는 것 외에도, 날짜와 시간을 서로 변환하거나 다른 표현 방식으로 전환하는 작업도 프로그래밍에서 자주 발생합니다. 이를 위해서는 다양한 라이브러리나 API를 사용할 수 있으며, 개발자들은 편의와 필요에 따라 선택할 수 있습니다.
3. 날짜를 계산하기 위해서는 기준 시간대를 고려해야합니다. 시간대의 차이를 처리하지 않으면, 다른 지역의 시간으로 계산되어 원하는 결과를 얻지 못할 수 있습니다.

## 참조:
- [Moment.js 라이브러리](https://momentjs.com/)
- [JavaScript의 Date 객체 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)