---
title:                "미래 또는 과거의 날짜 계산하기"
html_title:           "Javascript: 미래 또는 과거의 날짜 계산하기"
simple_title:         "미래 또는 과거의 날짜 계산하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하게 되는 이유는 우리 일상생활에서 다양한 이유로 날짜를 알아야 할 때가 있기 때문입니다. 예를 들어, 오늘이 어떤 요일인지, 기념일이 몇 일 뒤에 있는지 등을 알고 싶을 때 계산을 하게 되는데, 자바스크립트를 사용하면 간단하게 날짜를 계산할 수 있습니다.

## 하우 투

```Javascript
// 미래 날짜 계산
let currentDay = new Date(); // 오늘 날짜
let futureDay = currentDay.getDate() + 7; // 7일 뒤 날짜
currentDay.setDate(futureDay); // 미래 날짜로 설정
console.log(currentDay); // 출력: 현재로부터 7일 뒤의 날짜

// 과거 날짜 계산
let currentDate = new Date(); // 오늘 날짜
let pastDate = currentDate.getDate() - 7; // 7일 전 날짜
currentDate.setDate(pastDate); // 과거 날짜로 설정
console.log(currentDate); // 출력: 현재로부터 7일 전의 날짜
```

위의 예시는 오늘 날짜를 기준으로 미래 날짜와 과거 날짜를 간단하게 계산하는 방법을 보여줍니다. 날짜를 구하는 데 사용되는 함수들을 잘 활용하면 더욱 다양한 계산을 할 수 있으니, 여러 가지 예시를 통해 익혀보시기 바랍니다.

## 딥 다이브

날짜를 계산하는 데에는 많은 함수들이 있지만, 가장 기본적인 것은 `Date()` 객체입니다. 이 객체는 년, 월, 일, 시간 등 다양한 날짜 정보를 다룰 수 있습니다. 또한, `getDate()`, `setDate()`, `getMonth()`, `getYear()` 등 다양한 함수들을 이용하여 원하는 날짜를 계산할 수 있습니다. 자세한 내용은 공식 문서나 다른 블로그에서 참고하시기 바랍니다.

## 참고

- [JavaScript Date 객체 - MDN](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [JavaScript에서 날짜 계산하는 방법](https://engkimbs.tistory.com/766)