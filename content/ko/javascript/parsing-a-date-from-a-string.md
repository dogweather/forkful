---
title:                "문자열에서 날짜 분석하기"
html_title:           "Gleam: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

문자열에서 날짜를 파싱하는 것은 문자열 형태의 날짜 데이터를 JavaScript에서 사용가능한 `Date` 객체로 변환하는 과정입니다. 프로그래머들은 이런 변환을 통해 날짜 및 시간에 관한 연산을 수행하거나, 특정 형식으로 날짜를 출력하는 등의 작업을 할 때 이를 필요로 합니다.

## 어떻게:

```Javascript
let dateStr = "2022-03-31";
let dateObj = new Date(dateStr);
console.log(dateObj);
```
입력 : `"2022-03-31"`

출력 : `2022-03-30T15:00:00.000Z`

## 깊이 알아보기:

문자열에서 날짜를 파싱하게 된 배경은 초기 웹 개발에서는 날짜와 시간 데이터를 문자열 형태로 주고받는 것이 일반적이었기 때문입니다. 이런 방식은 다양한 문제점을 가지고 있었지만, 표준화되지 않은 초창기 웹 환경에서는 광범위하게 사용되었습니다.

문자열에서 날짜를 파싱하는 대안으로는 UNIX 타임스탬프를 사용하는 방법이 있습니다. 이 방식은 1970년 1월 1일 00시 00분 00초를 시작으로 경과한 시간을 밀리초 단위로 표현한 것입니다.

JavaScript 내부에서 `Date` 객체는 UTC를 기준으로 시간을 계산하며, `new Date()` 생성자는 다양한 형식의 날짜 문자열 파싱을 지원합니다.

## 참고자료:

1. MDN Web Docs - [Date 객체](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
2. W3Schools - [JavaScript Date Formats](https://www.w3schools.com/js/js_date_formats.asp)
3. Eloquent JavaScript - [Handling dates and time](https://eloquentjavascript.net/12_date.html)