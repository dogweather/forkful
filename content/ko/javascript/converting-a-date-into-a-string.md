---
title:                "Javascript: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 왜

자바스크립트 프로그래밍을 하면서 날짜를 문자열로 변환하는 방법에 대해 알고 싶은 이들이 있을 수 있습니다. 이 기능은 일상적으로 많이 사용되지는 않지만, 날짜를 다른 데이터와 함께 출력하거나 비교할 때 도움이 됩니다. 자바스크립트에서 날짜를 문자열로 변환하는 이유를 알아보겠습니다.

# 어떻게

자바스크립트에서 날짜를 문자열로 변환하는 방법에는 여러 가지가 있습니다. 우선, Date 객체의 toDateString() 메소드를 사용하여 날짜 객체를 문자열로 변환할 수 있습니다. 예를 들어,

```javascript
let today = new Date(); // 현재 날짜 객체 생성
let dateStr = today.toDateString(); // toDateString() 메소드 사용
console.log(dateStr); // "Mon May 31 2021"
```

또는 toISOString() 메소드를 사용하면 날짜를 ISO 형식의 문자열로 변환할 수도 있습니다. 예를 들어,

```javascript
let today = new Date(); // 현재 날짜 객체 생성
let dateStr = today.toISOString(); // toISOString() 메소드 사용
console.log(dateStr); // "2021-05-31T05:00:00.000Z"
```

또는 Moment.js와 같은 라이브러리를 사용하여 더 다양한 날짜 포맷을 생성할 수도 있습니다.

# 딥 다이브

날짜를 문자열로 변환하는 방법은 자바스크립트에서 기본적으로 제공하는 toDateString() 메소드를 사용하는 것이 가장 간단합니다. 그러나 이를 사용할 때 주의할 점이 있습니다. 바로 로컬 시간대를 기준으로 변환된다는 것입니다. 따라서 다른 사용자들이 다른 시간대에 있을 경우, 변환된 문자열이 다를 수 있습니다. 이를 해결하기 위해서는 Moment.js와 같은 라이브러리를 사용하거나 Date 객체의 메소드를 활용하여 UTC 시간대를 기준으로 날짜를 변환하는 방법을 알아보는 것이 좋습니다.

# 이어보기

- [Date 객체 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js 라이브러리](https://momentjs.com/)
- [자바스크립트 날짜 다루기](https://www.daleseo.com/js-date-basic/)