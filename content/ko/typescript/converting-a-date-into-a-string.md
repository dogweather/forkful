---
title:                "날짜를 문자열로 변환하기"
html_title:           "TypeScript: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 데에 어떤 이유로 참여할까요? JavaScript에서 날짜를 바로 출력하는 경우, 기본적으로 영문으로 표시되어 나오지만 TypeScript를 통해 날짜를 원하는 형태로 바꿀 수 있습니다.

## 방법

```TypeScript
const date = new Date(); // 현재 날짜 가져오기

// 날짜와 시간을 원하는 형식으로 바꾸기
const stringDate = date.toLocaleString(
  'en-US', // 날짜 포맷 언어 설정
  {
    weekday: 'long', // 요일 긴 이름으로 표시
    year: 'numeric',
    month: 'long', // 월 긴 이름으로 표시
    day: 'numeric',
    hour: 'numeric',
    minute: 'numeric',
    hour12: true, // 12시간 형식으로 표시
  }
);
console.log(stringDate); // 출력 예시: Friday, May 21, 2021, 7:23 PM
```

위의 예시 코드에서는 `Date` 객체를 사용하여 현재 날짜와 시간을 가져오고, `toLocaleString` 메서드를 사용하여 원하는 형식으로 날짜를 변환합니다. 이때 옵션으로 언어와 포맷을 설정할 수 있습니다. 자세한 포맷 옵션은 [MDN 문서](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString)를 참고하세요.

## 깊이 알아보기

매년 다른 유저가 사용하는 다른 언어를 고려하여 날짜를 출력해야 하는 다국어 웹 애플리케이션을 개발하는 경우, `Date` 객체와 `toLocaleString` 메서드를 적절히 활용하여 다양한 언어로 날짜를 출력할 수 있습니다. 또한 날짜 포맷에 관한 자세한 정보는 [ECMA-262 (JavaScript 언어 사양의 코어)](https://www.ecma-international.org/publications-and-standards/standards/ecma-262/)를 참고할 수 있습니다.

## 관련 문서

- [MDN 문서: Date.prototype.toLocaleString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString)
- [TypeScript 문서: Date](https://www.typescriptlang.org/docs/handbook/standard-library.html#date)
- [ECMA-262 (JavaScript 언어 사양의 코어)](https://www.ecma-international.org/publications-and-standards/standards/ecma-262/)