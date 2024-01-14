---
title:                "Javascript: 미래나 과거의 날짜 계산하기"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 일을 할 이유는 다양합니다. 예를 들어, 미래의 날짜를 계산하여 중요한 일정을 파악하고 준비할 수 있습니다. 또는 과거 날짜를 계산하여 기념일을 기억하고 축하할 수 있습니다. 이러한 일들을 자동화하고 싶은 경우, 자바스크립트 프로그래밍을 활용하면 편리합니다.

## 어떻게

```Javascript
// 현재 날짜와 시간을 저장하는 변수
let today = new Date();

// 미래 날짜 계산하기
let futureDate = new Date(today.getFullYear(), today.getMonth(), today.getDate() + 7);

// 과거 날짜 계산하기
let pastDate = new Date(today.getFullYear(), today.getMonth(), today.getDate() - 30);

// 날짜 출력하기
console.log(`다음 주 일요일은 ${futureDate} 입니다.`);
console.log(`30일 전의 날짜는 ${pastDate} 입니다.`);
```

위의 예제 코드는 현재 날짜와 시간을 변수에 저장하고, 그것을 기반으로 미래 날짜를 계산하는 방법과 과거 날짜를 계산하는 방법을 보여줍니다. ```new Date()``` 함수를 사용하여 날짜와 시간을 생성하고, 이를 활용하여 새로운 변수에 저장합니다. 그리고 기존의 변수를 이용하여 원하는 날짜를 계산하여 출력하는 방식으로 동작합니다.

## 딥 다이브

날짜와 시간을 다루는 것은 프로그래밍에서 매우 중요한 기능입니다. 이를 계산하기 위해서는 내부적으로 일어나는 일에 대한 이해가 필요합니다. 자바스크립트에서는 내부 시간값을 밀리초 단위로 처리합니다. 따라서 미래 날짜나 과거 날짜를 계산할 때도 이러한 시간 단위를 기준으로 계산해야 합니다. 또한 Date 객체에서 제공하는 다양한 메소드를 활용하면 날짜를 보다 쉽게 다룰 수 있습니다.

## 관련 정보

- [MDN 자바스크립트 날짜와 시간 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [자바스크립트에서 시간 다루기](https://snackaholic.tistory.com/115)
- [날짜와 시간 다루기](https://jay-ji.tistory.com/30)