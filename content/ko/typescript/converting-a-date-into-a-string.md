---
title:    "TypeScript: 날짜를 문자열로 변환하기"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 것이 왜 유용한지에 대해 궁금할 수 있습니다. JavaScript에서는 기본적으로 `Date` 객체를 사용하여 현재 날짜와 시간을 쉽게 가져올 수 있지만, 이러한 객체는 사용자에게 읽기 쉽게 날짜를 표현하는 것이 어려울 수 있습니다. 따라서 날짜를 문자열로 변환하면 적절한 형식으로 표현할 수 있어 더 쉽게 사용할 수 있습니다.

## 하는 방법

일단 TypeScript로 날짜를 문자열로 변환하는 방법을 살펴보겠습니다. 다음과 같이 코드를 작성할 수 있습니다.

```TypeScript
let date = new Date(); // 현재 날짜와 시간을 가져옴

// 원하는 형식으로 지정
let year = date.getFullYear();
let month = date.getMonth() + 1;
let day = date.getDate();
let hour = date.getHours();
let minute = date.getMinutes();
let second = date.getSeconds();

// 날짜를 YYYY-MM-DD HH:mm:ss 형식의 문자열로 변환
let dateString = `${year}-${month}-${day} ${hour}:${minute}:${second}`;

console.log(dateString); // 예시 출력: 2021-08-10 15:30:00
```

위 코드에서 `getMonth()` 메서드를 사용할 때 +1을 해주는 이유는 JavaScript의 `Date` 객체에서 `getMonth()` 메서드는 0부터 시작하기 때문입니다.

## 깊이 파헤치기

날짜를 문자열로 변환하는 방법에 대해 더 깊이 들어가보겠습니다. 날짜를 원하는 형식으로 지정하는 것 이외에도, TypeScript에서는 `Date` 객체를 다양한 문자열 형식으로 변환하는 방법을 제공합니다.

먼저 `toDateString()` 메서드는 영문으로 날짜를 표현해줍니다. `toTimeString()` 메서드는 시간을 영문으로 표현해줍니다. `toLocaleDateString()` 메서드는 로케일에 맞는 날짜 형식으로 변환해줍니다.

그리고 날짜와 시간의 각 부분을 따로 가져오는 것도 가능합니다. `getFullYear()`은 년도를, `getMonth()`는 월을, `getDate()`는 일을, `getHours()`는 시간을, `getMinutes()`는 분을, `getSeconds()`는 초를 각각 가져올 수 있습니다.

`getUTCFullYear()`은 UTC 기준 년도를 가져옵니다. UTC란 Universal Time Coordinated의 약자로, 세계 표준 시간을 나타내는 기준 시간대를 말합니다.

더 많은 문자열 변환 메서드에 대해서는 [TypeScript 공식 문서](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-2.html#string-to-enum-conversions)를 참고하시길 바랍니다.

## 더 보기

- [typescriptlang.org](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-2.html#string-to-enum-conversions)
- [developer.mozilla.org](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date) (영문)
- [tutorialsteacher.com](https://www.tutorialsteacher.com/typescript/typescript-date) (영문)