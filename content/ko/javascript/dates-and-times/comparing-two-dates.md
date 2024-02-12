---
title:                "두 날짜 비교하기"
aliases:
- /ko/javascript/comparing-two-dates/
date:                  2024-01-20T17:33:21.962496-07:00
model:                 gpt-4-1106-preview
simple_title:         "두 날짜 비교하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

자바스크립트에서 두 날짜를 비교하는 것은 특정한 시간이 전인지, 후인지, 같은지를 확인하는 과정입니다. 프로그래머들은 기간을 측정하거나, 날짜 기반 의사결정을 내리기 위해 이를 사용합니다.

## How to: (어떻게 하나요?)

```Javascript
// 두 날짜 객체 생성
const date1 = new Date('2023-04-01T00:00:00');
const date2 = new Date('2023-04-02T00:00:00');

// 날짜 비교
console.log(date1 < date2); // true, date1이 date2보다 이릅니다
console.log(date1 > date2); // false, date1이 date2보다 늦습니다
console.log(date1.getTime() === date2.getTime()); // false, 두 날짜는 같지 않습니다
```

## Deep Dive (심층 분석)

- **역사적 맥락**: 자바스크립트의 Date 객체는 1995년 언어가 처음 만들어질 때부터 존재했습니다. 
- **대안**: 라이브러리 없이 날짜를 비교할 수 있지만, moment.js와 같은 라이브러리를 사용하면 편리합니다.
- **구현 세부 사항**: 자바스크립트에서는 `getTime()` 메서드를 사용해 유닉스 타임스탬프로 변환하여 날짜를 비교합니다. 이 값은 1970년 1월 1일부터의 밀리초 수를 나타냅니다.

## See Also (더 보기)

- [Date 객체 - MDN Web Docs](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [Date-fns library](https://date-fns.org/)
