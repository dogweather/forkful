---
title:                "날짜를 문자열로 변환하기"
html_title:           "Arduino: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 필요한가?
날짜를 문자열로 변환하는 것은 날짜 데이터를 텍스트 형태로 표현하는 방법입니다. 이를 통해, 프로그래머들은 사용자에게 날짜를 더 쉽게 이해할 수 있는 형태로 제공하거나, 파일 이름 또는 로그에 날짜를 간결하게 적용할 수 있습니다.

## 어떻게 하면 되는가:
```Javascript
// 새로운 Date 객체를 생성합니다.
let date = new Date();

// 날짜를 문자열로 변환합니다.
let dateString = date.toString();

// 결과를 출력합니다.
console.log(dateString);
```
이 코드를 실행하면, 현재의 날짜와 시간을 포함한 줄이 출력됩니다.

## 깊이 들여다보기
과거에는 날짜를 문자열로 변환하는 것이 JavaScript에서 직접적으로 지원되지 않았습니다. 대신, 각 날짜 구성 항목(예: 연도, 월, 일)을 수동으로 문자열로 변환하고 이를 합쳤습니다.

하지만, 현재는 Date 객체의 ‛toString()’ 메소드를 사용해 날짜를 문자열로 손쉽게 변환할 수 있습니다. 여기에는 브라우저 간의 일관성을 보장하는 여러가지 표준화된 포맷이 포함되어 있습니다.

또한, 다른 구현 방법으로 ‛toLocaleString()’ 메소드를 사용하여 날짜를 사용자의 로케일에 적합한 문자열로 변환하는 방법도 있습니다.

## 참고 자료
- [MDN Web Docs: Date](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [JavaScript Info: Date and Time](https://javascript.info/date)
- [Stack Overflow: How to convert JavaScript Date to String format?](https://stackoverflow.com/questions/3066586/get-string-in-yyyymmdd-format-from-js-date-object)