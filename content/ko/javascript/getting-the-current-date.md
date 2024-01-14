---
title:    "Javascript: 현재 날짜 가져오기"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

날짜와 시간은 우리 일상에서 매우 중요합니다. 우리는 시간을 추적하고 일정을 관리하며 생일과 기념일을 기억합니다. 이러한 이유로 인해, 프로그래밍에서도 현재 날짜를 얻는 것이 중요합니다. 현재 날짜를 얻는 작업은 많은 프로그램에서 사용되며 다양한 목적으로 활용될 수 있습니다. 그러므로 현재 날짜를 얻는 방법을 배우는 것은 매우 유용합니다.

## 어떻게

자바스크립트에서 현재 날짜를 얻는 방법은 매우 간단합니다. Date 객체를 사용하여 간단한 코드로 현재 날짜를 얻을 수 있습니다. 다음은 이 작업을 수행하는 간단한 예제 코드입니다.

```Javascript
let today = new Date();
console.log(today);
```
출력 결과는 다음과 같을 것입니다.

```Javascript
Thu Oct 07 2021 14:41:56 GMT+0900 (Korean Standard Time)
```

Date 객체에는 다양한 메소드가 있으며, 이를 사용하여 원하는 형식으로 날짜를 출력할 수도 있습니다. 예를 들어, 원하는 형식으로 날짜를 출력하는 코드는 다음과 같을 수 있습니다.

```Javascript
let today = new Date();
let day = today.getDate();
let month = today.getMonth() + 1;
let year = today.getFullYear();
console.log(`${year}-${month}-${day}`);
```
출력 결과는 다음과 같을 것입니다.

```Javascript
2021-10-7
```

이와 같이 Date 객체를 활용하여 얻은 현재 날짜는 다양한 프로그램에서 사용될 수 있습니다.

## 딥 다이브

Date 객체의 메소드들은 더욱 다양한 형태의 출력을 지원합니다. 이 중에서도 가장 널리 사용되는 메소드는 getTime() 메소드입니다. 이 메소드는 1970년 1월 1일부터 현재까지 경과한 밀리초를 반환합니다. 이 밀리초 값은 다양한 계산에 활용될 수 있습니다. 또한, Date 객체는 날짜뿐만 아니라 시간도 가져올 수 있습니다. Date 객체에서 시간을 가져오는 메소드 중 가장 널리 사용되는 것은 getHours(), getMinutes(), getSeconds() 메소드입니다.

## 더 보기

- [MDN Date 객체](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [JS Date 객체에 대한 재미있는 사실들](https://www.w3schools.com/js/js_date_fun.asp)
- [다양한 Date 객체 활용 예시](https://blog.bitsrc.io/7-javascript-date-snippets-you-should-know-361861df287f)