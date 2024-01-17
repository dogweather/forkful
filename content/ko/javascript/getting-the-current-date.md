---
title:                "현재 날짜 받아오기"
html_title:           "Javascript: 현재 날짜 받아오기"
simple_title:         "현재 날짜 받아오기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

JavaScript의 현재 버전을 사용하는 프로그래머에게는 유용한 기능 중 하나가 현재 날짜를 얻는 것입니다. 이 기능을 사용하면 현재 시간을 정확하게 파악할 수 있고, 이를 이용해서 다양한 작업을 수행할 수 있습니다.

## What & Why?

현재 날짜를 얻는다는 것은 프로그래밍에서 현재 시간을 불러오는 것을 말합니다. 이 기능을 사용하면 다양한 작업에 활용할 수 있으며, 예를 들어 현재 날짜를 기준으로 이벤트를 계산할 수 있습니다. 프로그래머들은 이 기능을 사용해서 정확한 시간을 파악하고, 이를 바탕으로 다른 작업을 수행할 수 있습니다.

## How to:

다음은 JavaScript를 사용해서 현재 날짜를 얻는 방법입니다.

```javascript
// 현재 날짜 생성
var currentDate = new Date();

console.log(currentDate); // 예시 출력: Fri Sep 10 2021 11:00:00 GMT+0900 (한국 표준시)
```

위의 코드를 실행하면 콘솔에 현재 날짜와 시간이 출력됩니다. 날짜와 시간을 원하는 형식으로 출력하고 싶다면 다음과 같은 코드를 사용할 수 있습니다.

```javascript
// 현재 날짜를 "YYYY년 MM월 DD일" 형식으로 출력
var currentDate = new Date();

var year = currentDate.getFullYear();
var month = (currentDate.getMonth() + 1).toString().padStart(2, '0');
var day = currentDate.getDate().toString().padStart(2, '0');

console.log(year + '년 ' + month + '월 ' + day + '일'); // 예시 출력: 2021년 09월 10일
```

## Deep Dive:

JavaScript는 1995년 넷스케이프 커뮤니케이션즈에서 발표된 스크립트 언어입니다. 이 언어는 웹 페이지에 동적인 기능을 제공하기 위해 만들어졌습니다. 현재 날짜를 얻는 기능은 Date 객체의 메소드인 `getDate()`와 `getMonth()` 등을 사용하여 이루어집니다. 또한, Node.js 라이브러리를 사용하면 서버 측에서도 현재 날짜를 얻을 수 있습니다.

JavaScript와 달리 다른 프로그래밍 언어에서는 현재 날짜를 얻는 방법이 조금 다를 수 있습니다. 예를 들어, Python에서는 `datetime` 모듈을 사용하고, Java에서는 `Calendar` 클래스를 사용합니다.

## See Also:

- [JavaScript 공식 문서 - Date 객체](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [w3schools.com - JavaScript Date 객체](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [Tutorialspoint - JavaScript 현재 날짜 가져오기](https://www.tutorialspoint.com/How-to-get-current-date-in-JavaScript)