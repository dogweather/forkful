---
title:                "문자열에서 날짜 구문 분석"
html_title:           "Javascript: 문자열에서 날짜 구문 분석"
simple_title:         "문자열에서 날짜 구문 분석"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 해야하나요?
문자열에서 날짜를 분석해 내는 것은 프로그래머가 문자열에서 원하는 정보를 추출하고 처리할 수 있도록 도와줍니다. 날짜를 다루는 경우에는 특히 필요한 기술이며, 이를 통해 날짜 데이터를 정확하게 처리할 수 있습니다.

## 어떻게 하나요?
날짜를 문자열에서 추출하는 방법에는 여러 가지가 있습니다. 다음은 Javascript를 이용해 날짜를 추출하는 예시입니다.

```
// 문자열에서 날짜 추출하기
const string = "2021-05-14";
const date = new Date(string);

// 원하는 형식으로 날짜 출력하기
console.log(date.toLocaleDateString("ko-KR")); // "2021. 5. 14."
```

## 깊게 들어가보기
날짜를 문자열에서 추출하는 기술은 아주 오랜 역사를 가지고 있습니다. 이전에는 문자열을 직접 분석하는 방식을 사용했지만, 지금은 더 간편하고 정확한 방법인 내장 함수를 이용합니다. 다른 프로그래밍 언어에서도 비슷한 기능을 제공하며, 날짜 형식에 맞추어 정확한 데이터를 추출할 수 있습니다.

## 더 알아보기
이 문제에 대한 해결 방법은 다양합니다. 자세한 내용을 알고 싶다면 다음 링크를 참고해보세요.

- [MDN 웹 문서: Date 객체](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Javascript에서 날짜 다루기](http://www.incodom.kr/Javascript/AD_0610.html)