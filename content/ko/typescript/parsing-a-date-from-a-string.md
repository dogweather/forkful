---
title:                "문자열에서 날짜 분석하기"
html_title:           "Gleam: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 뭐하고, 왜?

문자열에서 날짜를 파싱하는 것은, 특정 형식의 문자열을 날짜 데이터로 변환하는 프로그래밍 기술입니다. 이는 사용자가 입력한 날짜 정보를 프로그램에서 사용 가능한 형식으로 만들기 위해 자주 사용됩니다.

## 어떻게 하면 될까요:

아래에 TypeScript를 사용하여 문자열에서 날짜를 파싱하는 방법에 대한 예를 들어 설명하겠습니다.

```TypeScript
let inputString = "2022-08-02";
let parsedDate = new Date(inputString);

console.log(parsedDate);
```

위에 코드는 "2022-08-02"라는 문자열을 Date 객체로 변환하고 출력합니다.

```Shell
2022-08-02T00:00:00.000Z
```

## 디테일:

먼저, 문자열에서 날짜 파싱은 오래된 문제입니다. 날짜 문자열의 포맷은 다양하며, 이를 파싱하는 방법은 여러가지가 있습니다. `new Date()` 밖에 없는 것은 아닙니다. `Date.parse()`나 라이브러리(Library) such as Moment.js, date-fns 등을 사용할 수도 있습니다.

그럼에도 불구하고 `new Date()` 함수는 상대적으로 간단하며 다양한 날짜 포맷을 지원합니다. 이는 ISO 8601 날짜 문자열 표현법을 기본으로 지원하므로 이번 예제에서는 이 방법을 사용하였습니다.

## 참조:

- [MDN Web Docs - Date](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](http://momentjs.com/)
- [date-fns](https://date-fns.org/)