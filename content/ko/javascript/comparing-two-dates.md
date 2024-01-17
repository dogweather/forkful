---
title:                "두 날짜 비교하기"
html_title:           "Javascript: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

두 날짜를 비교한다는 것은 무엇인가요? 그리고 프로그래머들이 왜 이렇게 하는 걸까요? 날짜를 비교하는 것은 간단하지만 중요한 작업입니다. 우리는 날짜를 비교해서 이전일인지, 이후일인지, 또는 동일한 날짜인지 알 수 있기 때문입니다. 이를 통해 우리는 프로그램을 더욱 똑똑하게 만들고, 정확한 결과를 얻을 수 있습니다.

## 하는 법:

### 날짜 비교하기

```Javascript
const today = new Date(); // 오늘 날짜를 가져옵니다.

const yesterday = new Date("2021-09-14"); // 2021년 9월 14일의 날짜 정보를 담은 객체를 만듭니다.

if (yesterday < today) { // 어제의 날짜가 오늘보다 이전인지 비교합니다.
    console.log("어제의 날짜는 오늘보다 이전입니다.");
}
```

### 시간 비교하기

```Javascript
const currentTime = new Date(); // 현재 시간을 가져옵니다.

const deadline = new Date("2021-10-01"); // 2021년 10월 1일 00:00:00의 시간 정보를 담은 객체를 만듭니다.

if (currentTime.getTime() > deadline.getTime()) { // 현재 시간이 데드라인 시간보다 늦은지 비교합니다.
    console.log("데드라인 시간을 지났습니다.");
}
```

### 두 날짜가 동일한지 비교하기

```Javascript
const a = new Date("2021-09-01"); 
const b = new Date("2021-09-01");

if (a.getTime() == b.getTime()) { // a와 b의 날짜가 동일한지 비교합니다.
    console.log("a와 b는 동일한 날짜입니다.");
}
```

## 깊게 들어가보기:

### 역사적 배경

날짜를 비교하는 방법은 프로그래밍에서 매우 오래된 기술 중 하나입니다. 초기 컴퓨터 시스템은 정확한 날짜와 시간을 추적하는 기능이 없었기 때문에 날짜를 비교하는 것은 매우 중요한 작업이었습니다. 오늘날에도 여전히 프로그래밍에서 중요한 부분을 차지하고 있습니다.

### 다른 방법들

날짜를 비교하는 다른 방법으로는 라이브러리를 사용하는 것이 있습니다. Moment.js와 같은 라이브러리는 날짜를 더욱 쉽게 다룰 수 있는 유용한 기능들을 제공합니다. 하지만 Javascript에 내장된 Date 객체를 사용해도 충분히 날짜를 비교할 수 있습니다.

### 구현 방법

날짜를 비교하는 가장 기본적인 방법은 getTime() 메소드를 사용하는 것입니다. 이 메소드는 날짜 정보를 밀리초로 변환해줍니다. 이를 통해 두 날짜를 숫자로 비교할 수 있게 됩니다.

## 더 알아보기:

[MDN Date 객체 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)

[Moment.js 라이브러리](https://momentjs.com/)

[Date 객체 비교하기 (예제 포함)](https://www.w3schools.com/js/js_dates_compare.asp)