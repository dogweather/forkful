---
title:                "자료에서 날짜 추출하기"
html_title:           "TypeScript: 자료에서 날짜 추출하기"
simple_title:         "자료에서 날짜 추출하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇이고 왜 할까요?
날짜를 문자열로부터 파싱하는 것은 문자열에서 날짜를 추출하는 것을 말합니다. 이를 하는 이유는 프로그래머들이 날짜를 처리하지 않고 날짜 형식에 따라 코드를 작성할 수 있도록 하기 위해서입니다.

## 방법:
다음은 TypeScript에서 문자열로부터 날짜를 추출하는 방법 예제와 출력 예시입니다:

```TypeScript

// "2020-09-21" 형식으로 문자열이 주어졌을 때, 날짜 형식으로 변환하는 방법`
const dateString = "2020-09-21";
const date = new Date(dateString);
console.log(date); // Mon Sep 21 2020 00:00:00 GMT+0900 (한국 표준시)

// "Sep 21, 2020" 형식으로 문자열이 주어졌을 때, 날짜 형식으로 변환하는 방법
const dateString = "Sep 21, 2020";
const date = new Date(dateString);
console.log(date); // Mon Sep 21 2020 00:00:00 GMT+0900 (한국 표준시)

```

## 깊이 파보기:
날짜를 문자열로부터 파싱하는 것은 날짜 형식이 여러 가지이기 때문에 중요한 과정입니다. 과거에는 날짜 형식이 표준화되지 않았기 때문에 프로그래머들이 다른 형식을 사용하기도 했습니다. 그러나 현재는 날짜 형식이 표준화되어 있기 때문에 이 과정은 매우 중요하지 않지만 여전히 필요한 경우가 있습니다. 또 다른 대안으로는 Moment.js와 같은 라이브러리를 사용하여 날짜 형식 파싱을 수행할 수 있습니다. 구현 세부 정보로는 날짜를 추출하기 전에 문자열을 검증하고 유효한 날짜 형식인지 확인해야 합니다.

## 관련 자료:
- [TypeScript Date 문서](https://www.typescriptlang.org/docs/handbook/declarationspaces.html)
- [Moment.js 날짜 파싱 가이드](https://momentjs.com/docs/#/parsing/)