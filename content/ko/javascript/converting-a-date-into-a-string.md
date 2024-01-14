---
title:                "Javascript: 날짜를 문자열로 변환하기"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜?

개발자의 입장에서 날짜와 시간을 처리하는 것은 매우 중요합니다. 또한, 날짜와 시간을 문자열로 변형하는 것은 많은 경우에 필요합니다. 이 블로그 글에서는 날짜를 문자열로 변환하는 방법을 알아보고, 이를 왜 해야 하는지 알아보겠습니다.

## 어떻게?

```Javascript
const today = new Date();

// Date 객체를 문자열로 변환하기
const dateAsString = today.toString();
console.log(dateAsString); // "Tue Sep 28 2021 17:30:47 GMT+0900 (Korea Standard Time)"

// 날짜 포맷을 원하는 형식으로 지정하기 
const options = { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' };
const customDate = today.toLocaleDateString('ko-KR', options);
console.log(customDate); // "2021년 9월 28일 화요일"

// 다양한 날짜 포맷 예시
const date1 = today.toLocaleDateString('en-US'); 
console.log(date1); // "9/28/2021"

const date2 = today.toLocaleDateString('ja-JP'); 
console.log(date2); // "2021/9/28"

const date3 = today.toLocaleDateString('fr-FR'); 
console.log(date3); // "28/09/2021"

```

위의 예시 코드를 보면, ```Date``` 객체의 ```toString()``` 메소드를 사용하면 그대로 날짜 객체의 문자열 표현을 얻을 수 있습니다. 또한, 원하는 형식으로 날짜 포맷을 지정할 수도 있습니다. 이는 다국어 지원이 필요한 경우 유용하게 사용할 수 있습니다.

## 딥 다이브

자바스크립트의 ```Date``` 객체는 날짜와 시간을 다루는 매우 유용한 기능을 제공합니다. 이 객체를 사용하면, 날짜와 시간을 이용한 계산이나 표현이 매우 편리해집니다. 또한, 문자열로 변환하면서 날짜 포맷을 지정할 수 있다는 것도 큰 장점입니다. 따라서, 개발자들은 자주 사용하는 기능 중 하나인 날짜를 문자열로 변환하는 기능을 잘 숙지할 필요가 있습니다.

## See Also

- [MDN Date](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [DateFormat.js](https://formatjs.io/docs/core-concepts/icu-syntax/)
- [Convert Date to String in JavaScript](https://stackabuse.com/how-to-convert-a-date-to-string-in-javascript/)