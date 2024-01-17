---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "Javascript: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

날짜를 앞 또는 뒤로 계산하는 것은 특정 날짜를 기준으로 했을 때, 그 날짜의 이전이나 이후의 날짜를 알아내는 것을 말합니다. 프로그래머는 이것을 주로 날짜와 관련된 작업을 수행할 때 사용하며, 예를 들어 특정 날짜의 이전과 이후의 이벤트를 계산하는 등의 경우에 유용합니다.

## 방법:

### 특정 시간 이후의 날짜 계산하기:

```Javascript
// 현재 시간
let now = new Date();
// 현재 시간에 1년과 5일을 더한 날짜 계산
let futureDate = new Date(now.getFullYear() + 1, now.getMonth(), now.getDate() + 5);
console.log(futureDate);
// 출력 결과: 다음 해의 동일한 달과 같은 날짜를 기준으로 5일을 더한 날짜가 표시됨
```

### 특정 시간 이전의 날짜 계산하기:

```Javascript
// 현재 시간
let now = new Date();
// 현재 시간에서 1년과 5일을 뺀 날짜 계산
let pastDate = new Date(now.getFullYear() - 1, now.getMonth(), now.getDate() - 5);
console.log(pastDate);
// 출력 결과: 이전 해의 동일한 달과 같은 날짜를 기준으로 5일을 뺀 날짜가 표시됨
```

## 깊이 알아보기:

### 역사적 배경:

날짜를 계산하는 방법은 고대부터 존재했습니다. 로마 시대에는 캘린더를 사용하여 날짜를 계산했고, 이후 많은 문화에서도 현재 사용되는 그레고리오력을 개발하게 됩니다. 프로그래밍에서도 이와 같은 날짜 계산이 필요하여 많은 언어들에서 날짜와 관련된 내장 함수를 제공합니다.

### 대안:

날짜를 계산하는 방법에는 다양한 대안이 존재합니다. 예를 들어 Moment.js와 같은 자바스크립트 라이브러리를 사용할 수 있으며, 이 라이브러리를 사용하면 훨씬 쉽게 날짜 계산을 수행할 수 있습니다.

### 구현 세부 사항:

자바스크립트에서는 Date 객체를 사용하여 날짜와 관련된 작업을 수행할 수 있습니다. Date 객체의 생성자에는 다양한 인자를 전달하여 원하는 날짜를 생성할 수 있으며, 기본적으로 현재 시간을 기준으로 하게 됩니다. 이후 날짜 계산을 위해 내장된 getYear(), getMonth(), getDate() 함수를 사용하여 각각 연도, 월, 일을 반환할 수 있습니다.

## 관련 자료:

- [MDN Web Docs - Date 객체](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [로마 시대의 캘린더](https://ko.wikipedia.org/wiki/%EC%BA%98%EB%A6%B0%EB%8D%94)