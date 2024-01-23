---
title:                "문자열에서 날짜 파싱하기"
date:                  2024-01-20T15:37:03.029874-07:00
html_title:           "Arduino: 문자열에서 날짜 파싱하기"
simple_title:         "문자열에서 날짜 파싱하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
문자열에서 날짜를 파싱하는 것은 문자열 형식의 데이터를 날짜 객체로 변환하는 과정입니다. 프로그래머들은 여러 소스에서 온 날짜 데이터를 통일된 형식으로 처리하거나 날짜 연산을 수행하기 위해 이를 수행합니다.

## How to: (어떻게 하나요?)
```javascript
// 날짜 문자열로부터 날짜 객체 생성
const dateString = "2023-04-01T12:30:00Z";
const dateObject = new Date(dateString);

console.log(dateObject);
// 출력: Sat Apr 01 2023 21:30:00 GMT+0900 (한국 표준시)

// 문자열 형식이 표준 ISO 형식이 아닌 경우 Date.parse를 사용하여 타임스탬프를 얻은 후 Date 객체 생성
const customDateString = "01/04/2023 12:30 PM";
const timestamp = Date.parse(customDateString);
const customDateObject = new Date(timestamp);

console.log(customDateObject);
// 출력: Sat Apr 01 2023 12:30:00 GMT+0900 (한국 표준시)
```

## Deep Dive (심화 탐구)
날짜 파싱은 초기 웹 개발에서부터 중요한 부분이었습니다. 초기에는 개발자들이 직접 문자열에서 년, 월, 일을 추출하고 Date 객체를 만들었죠. 이후 표준화된 Date 생성자와 메소드들이 도입되었습니다.

- **표준 ISO 형식**: JavaScript는 ISO 8601 형식 (`YYYY-MM-DDTHH:mm:ss.sssZ`)의 날짜 문자열을 직접 처리할 수 있습니다.
- **비표준 형식**: 다른 형식을 파싱할 때는 주의가 필요합니다. `Date.parse()`는 다양한 문자열 포맷에 대해 유연하긴 하나, 브라우저마다 차이가 날 수 있습니다.
- **라이브러리 사용**: Moment.js와 같은 날짜 관리 라이브러리를 사용하면 더 많은 형식과 복잡한 시간연산을 쉽게 처리할 수 있습니다. 하지만, 라이브러리는 파일 크기가 커지는 단점이 있을 수 있습니다.

날짜 파싱의 구현은 크게 두 가지 방식으로 나뉩니다. 하나는 내장된 `Date` 생성자와 메소드를 사용하는 것이고, 다른 하나는 문자열을 직접 분석하여 날짜 컴포넌트를 추출한 후 Date 객체를 생성하는 것입니다.

## See Also (더 알아보기)
- [MDN Date Reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date): JavaScript의 Date 객체에 대한 상세한 정보.
- [ISO 8601 on Wikipedia](https://en.wikipedia.org/wiki/ISO_8601): 날짜와 시간 표현에 대한 국제 표준을 설명하는 문서.
- [Moment.js Library](https://momentjs.com/): 다양한 날짜 파싱, 조작, 검증이 가능한 라이브러리 웹사이트.
