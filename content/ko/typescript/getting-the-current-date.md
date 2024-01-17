---
title:                "현재 날짜 가져오기"
html_title:           "TypeScript: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 현재 날짜란 무엇인가?

현재 날짜를 가져오는 것은 프로그래머가 현재 시간과 날짜를 가져오기 위해 사용하는 방법입니다. 이는 특히 사용자에게 현재 시간을 표시하거나, 일정한 기간이 지난 후 다시 실행해야 할 경우에 사용됩니다.

## 방법:

```TypeScript
const currentDate = new Date();
console.log(currentDate); // Sample output: Mon Mar 08 2021 19:07:15 GMT+0900 (Korean Standard Time)
```

위의 코드는 Date 객체를 생성하고, 그 객체를 사용하여 현재 날짜를 가져와 콘솔에 출력하는 예시입니다. TypeScript에서는 Date 객체를 사용하여 날짜와 시간을 다양한 형식으로 가져올 수 있습니다.

## 깊게 들어가보기:

현재 날짜를 가져오는 방법은 기본적으로 Date 객체를 생성하는 것입니다. 이 객체는 자바스크립트에서 제공하는 내장 객체이며, 날짜와 시간과 관련된 다양한 메서드를 제공합니다. 또한, moment.js와 같은 라이브러리를 사용하여 편리하게 현재 날짜를 가져올 수도 있습니다.

## 관련 자료:

- [Date 객체 - MDN 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [moment.js 라이브러리 - 공식 사이트](https://momentjs.com/)