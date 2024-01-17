---
title:                "날짜를 문자열로 변환하기"
html_title:           "Javascript: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
날짜를 문자열로 변환하는 것은 프로그래머들이 날짜를 다루는 방법 중 하나입니다. 날짜를 문자열로 변환하는 이유는 보다 쉽게 날짜를 저장하고 다룰 수 있기 때문입니다.

## 하는 방법:
```Javascript
// 날짜를 문자열로 변환하는 기본적인 방법
let today = new Date();
let dateString = today.toString();

console.log(dateString); // "Thu Jul 09 2020 09:30:41 GMT+0900 (GMT+09:00)"
```
```Javascript
// 원하는 형식으로 날짜를 문자열로 변환하는 예시
let today = new Date();
let options = { year: 'numeric', month: 'long', day: 'numeric' };
let dateString = today.toLocaleDateString('ko-KR', options);

console.log(dateString); // "2020년 7월 9일"
```

## 깊게 들어가보기:
1. 과거에는 컴퓨터의 시간은 1970년 1월 1일 이후로만 계산할 수 있었습니다. 이후에 추가된 기능으로 문자열을 다루는 메서드들이 나오게 되었고, 날짜를 문자열로 변환하는 메서드 역시 그 중 하나입니다.
2. 날짜를 문자열로 변환하는 다양한 방법이 존재합니다. 위에서 소개한 ```toString()``` 메서드 이외에도, ```toDateString()```, ```toLocaleDateString()``` 등의 메서드들이 있습니다. 각각의 메서드는 다른 형식으로 문자열을 반환하기 때문에, 사용 목적에 따라 맞는 메서드를 선택하여 사용하세요.
3. LocalDate가 도입되며, 날짜와 시간을 다루는 방식이 바뀌게 되었습니다. 따라서 날짜를 문자열로 변환할 때에도 LocalDate 객체의 메서드들을 이용해 원하는 형식으로 문자열을 반환할 수 있습니다.

## 관련 자료:
- [JavaScript의 Date 객체](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [날짜를 다루는 다양한 메서드의 사용법](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date/prototype)
- [Locale에 따라 다른 날짜와 시간 형식을 사용하는 방법](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)