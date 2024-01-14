---
title:                "TypeScript: 현재 날짜 가져오기"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

무엇때문에: 

누군가는 현재 날짜를 얻는 것에 참여하는 이유는 무엇일까요?

## Why
현재 날짜를 얻는 것이 공식적인 날짜 정보를 제공하고 시간 기능을 구현하는 데 매우 유용하기 때문입니다.

## How to
먼저, Date 객체를 생성하여 현재 날짜와 시간을 얻을 수 있습니다.

```TypeScript
let currentDateTime = new Date();
console.log(currentDateTime);
```

위의 코드를 실행하면 다음과 같은 출력이 나옵니다.

```bash
2021-07-21T07:09:36.072Z
```

현재 날짜와 시간이 ISO 8601 형식으로 출력되는 것을 볼 수 있습니다. 이제 다른 형식으로 날짜와 시간을 출력해보겠습니다.

```TypeScript
let currentDateTime = new Date();
let options = { year: "numeric", month: "long", day: "numeric", hour: "numeric", minute: "numeric", second: "numeric" };
let formattedDateTime = currentDateTime.toLocaleDateString("ko-KR", options);
console.log(formattedDateTime);
```

위의 코드를 실행하면 다음과 같은 출력이 나옵니다.

```bash
2021년 7월 21일 오전 4:09:36
```

위의 코드에서 `toLocaleDateString()` 메소드에 전달한 "ko-KR" 매개변수는 한국의 지역화 설정을 나타냅니다. 이를 통해 날짜와 시간을 한국어 형식으로 출력할 수 있습니다.

## Deep Dive
Date 객체의 `getDate()`, `getMonth()`, `getFullYear()` 메소드를 사용하여 각각 일, 월, 년 정보를 얻을 수 있습니다. 또한 `getHours()`, `getMinutes()`, `getSeconds()` 메소드를 사용하여 시, 분, 초 정보를 얻을 수도 있습니다. 이외에도 다양한 메소드와 옵션을 사용하여 원하는 형식으로 날짜와 시간을 출력할 수 있습니다.

## See Also
- [MDN Date 객체](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [MDN Intl.DateTimeFormat](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat)
- [Date 객체와 시간](https://code.visualstudio.com/docs/editor/integrated-terminal#_date-object-and-time)