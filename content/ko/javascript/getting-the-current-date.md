---
title:                "현재 날짜 가져오기"
html_title:           "C: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

현재 날짜를 알아내는 것은 프로그램이 오늘의 날짜를 인지할 수 있게 하는 방법입니다. 프로그래머는 사용자가 입력한 정보를 시간 순으로 정렬하거나, 정해진 기간 동안 특정 작업을 수행하게 하려는 이유로 이를 사용합니다.

## 어떻게 수행할까요:

JavaScript에서 현재 날짜 및 시간을 가져오려면, Date 객체를 사용합니다. 단순한 예제에서는 다음과 같이 사용할 수 있습니다.

```Javascript
let currentDate = new Date();
console.log(currentDate);
```

이 코드를 실행하면, 아래와 같은 출력을 확인할 수 있습니다.

```Javascript
2020-12-01T14:48:00.000Z
```

## 깊게 들어가보기:

JavaScript의 Date 객체는 1995년에 최초로 도입되었으며, 다른 많은 프로그래밍 언어와 마찬가지로 날짜 및 시간 조작을 위한 기능을 제공합니다. 

가능한 대안으로는 라이브러리를 사용하는 것이 있습니다. 예를 들어, `moment.js`는 Date 객체를 더욱 쉽게 조작할 수 있도록 해주는 인기 있는 라이브러리입니다. 

구현 세부 사항에 대해서는, JavaScript의 Date 객체는 1970년 1월 1일을 기점으로 밀리초 단위로 시간을 측정합니다. 이는 당시 Unix 시간 시스템이 사용되기 시작한 시각입니다.

## 참고하기:

더욱 깊게 공부하려면, 다음 자료를 참고하세요.
1. [Mozilla Developer Network (MDN)에서의 Date 객체 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
2. [JavaScript Tutorial의 날짜 및 시간 부분](http://javascript.info/date)
3. [moment.js 공식 홈페이지](https://momentjs.com/)