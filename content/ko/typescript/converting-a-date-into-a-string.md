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

## 무엇 & 왜?

타입스크립트에서 날짜를 문자열로 변환하는 것이란 무엇일까요? 프로그래머들이 이를 하는 이유가 무엇일까요? 날짜를 문자열로 변환하는 것은 날짜를 보다 쉽게 다루기 위해 날짜를 컴퓨터가 이해할 수 있는 형식으로 변경하는 것입니다. 이를 통해 날짜와 관련된 계산이나 조작을 더욱 간편하게 할 수 있습니다.

## 방법:

TypeScript에서 날짜를 문자열로 변환하는 방법을 알아보겠습니다.

#### Date 객체를 사용하는 방법:
```
const currentDate = new Date();
const dateString = currentDate.toString();
console.log(dateString);

// Output: Wed Jan 26 2022 17:03:45 GMT+0900 (GMT Standard Time)
```

#### toLocaleDateString() 메소드를 사용하는 방법:
```
const currentDate = new Date();
const dateString = currentDate.toLocaleDateString();
console.log(dateString);

// Output: 1/26/2022
```

## 깊이 파고들기:

날짜를 문자열로 변환하는 것은 프로그래밍 분야에서 매우 일반적인 작업입니다. 특히 날짜를 다양한 형식으로 표현할 수 있는 JavaScript의 Date 객체를 사용하는 경우 더욱 쉽게 할 수 있습니다. 또한 Moment.js와 같은 라이브러리를 사용해 날짜를 포맷팅하는 방법도 존재합니다.

## 관련 자료:

- [JavaScript Date 객체](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js 라이브러리](https://momentjs.com/)