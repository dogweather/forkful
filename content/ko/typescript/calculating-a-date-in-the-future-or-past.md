---
title:    "TypeScript: 미래나 과거의 날짜 계산하기"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 것에 참여하는 이유는 다양합니다. 예를 들어, 예약 시스템에서 날짜를 계산하여 사용자가 예약을 할 수 있는지 여부를 확인하는 데 사용할 수 있습니다.

## 하는 방법

```TypeScript
// 날짜를 계산하는 함수
function calculateDate(date: Date, days: number): Date {
    return new Date(date.getTime() + days * 24 * 60 * 60 * 1000);
}

// 2020년 8월 1일에 10일을 더한 날짜 계산
const result: Date = calculateDate(new Date(2020, 7, 1), 10);
console.log(result); // 2020-08-11T00:00:00.000Z
```

위 예제에서는 날짜와 더하려는 일 수를 매개변수로 받는 함수 `calculateDate`를 정의하고, `Date` 객체의 `getTime` 메소드를 이용해 계산된 날짜를 반환합니다. 또한 `console.log`를 이용해 계산된 날짜 객체를 출력해 보았습니다.

## 깊게 파고들기

위 예제에서는 날짜 계산에 사용된 간단한 방법을 보여줬지만, 실제로는 다양한 도구나 라이브러리를 이용해 더 정확하고 복잡한 날짜 계산을 할 수 있습니다. 예를 들어, Moment.js는 다양한 날짜 계산을 지원하는 강력한 JavaScript 라이브러리입니다. 또한 코드에서 이러한 날짜 계산을 많이 사용하는 경우에는 유틸리티 함수나 헬퍼 함수를 만들어 재사용성을 높일 수도 있습니다.

## 관련 링크

- [Moment.js 공식 문서 (한국어)](https://momentjs.com/locale/ko/)
- [Moment.js를 이용한 날짜 계산 예제 (블로그)](https://vectrumpost.com/programming/make-a-day-calculator-using-moment-js/)