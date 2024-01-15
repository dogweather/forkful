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

## 왜
현재 날짜를 얻는 것에 참여해야 할 이유는 무엇일까요? TypeScript에서 현재 날짜를 가져오는 방법을 알아보겠습니다.

## 하는 방법
```TypeScript
// Date 객체 생성
const date = new Date();

// 년, 월, 일 정보 가져오기
const year = date.getFullYear();
const month = date.getMonth() + 1;
const day = date.getDate();

// 출력 예제: 2020년 10월 5일
console.log(`${year}년 ${month}월 ${day}일`);
```

## 더 깊게 알아보기
Date 객체는 현재 날짜뿐만 아니라 다양한 날짜 정보를 제공해줍니다. `getDay()` 메소드를 사용하면 오늘이 주 중 몇 번째 날인지를 알 수 있고, `getTime()` 메소드를 사용하면 1970년 1월 1일 이후 경과한 밀리초를 출력할 수 있습니다.

## 참고자료
[MDN - Date](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)\
[typescriptlang.org - Date](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-2.html#the--object-rest-spread-operator)