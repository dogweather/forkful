---
title:    "Javascript: 미래나 과거의 날짜 계산하기"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 왜

날짜를 미래나 과거로 계산하는 것에 대해 관심이 생길 수도 있습니다. 예를 들어, 미래의 특정 날짜에 이벤트를 계획하거나 과거의 특정 날짜를 추억하고 싶을 수도 있습니다. 자바스크립트를 사용하면 간단하게 날짜를 계산할 수 있습니다.

## 어떻게

우선, JavaScript에서 `new Date()`를 사용하여 현재 날짜와 시간을 가져올 수 있습니다. 이후 `getFullYear()`을 사용하여 년도, `getMonth()`를 사용하여 월, `getDate()`을 사용하여 일을 가져올 수 있습니다. 하지만 이렇게 하면 현재 시간만을 가져오기 때문에, 미래나 과거의 날짜를 계산하려면 `setFullYear()` , `setMonth()` , `setDate()` 등의 메소드를 사용해야 합니다.

```Javascript
let currentDate = new Date();
let year = currentDate.getFullYear();
let month = currentDate.getMonth() + 1; //0부터 시작하기 때문에 1을 더해줍니다.
let day = currentDate.getDate();

//미래의 날짜 계산 예시
let futureDate = new Date();
futureDate.setFullYear(year + 1); //1년 후로 설정
futureDate.setMonth(month + 3); //3달 후로 설정
futureDate.setDate(day + 10); //10일 후로 설정

console.log(futureDate.getFullYear() + "년 " + futureDate.getMonth() + "월 " + futureDate.getDate() + "일");
// 2022년 1월 19일 
```

위의 예시에서는 `getFullYear()` , `getMonth()` , `getDate()` , `setFullYear()` , `setMonth()` , `setDate()` 등의 메소드를 사용하여 날짜를 계산했습니다. 이 외에도 `getTime()` , `getHours()` , `getMinutes()` , `getSeconds()` 등 다양한 메소드가 있으니 필요한 경우 찾아보시길 바랍니다.

## 깊이 들어가기

자바스크립트에서 날짜를 계산하는 것은 쉽지만, 여러분은 윤년을 반드시 고려해야 합니다. 윤년은 매 4년마다 발생하며, 이때 2월에 점프 데이가 추가됩니다. 따라서 날짜를 계산할 때 윤년을 고려해주지 않으면 정확한 결과를 얻지 못할 수 있습니다. 자바스크립트에서는 `getFullYear()` 메소드를 사용하여 해당 년도가 윤년인지 아닌지를 체크할 수 있습니다. 또한 `getTime()` 메소드를 사용하면 밀리초 단위로 날짜를 계산할 수 있습니다.

날짜 계산뿐만 아니라 날짜와 시간을 표시하는 방식도 다양합니다. 자바스크립트에서는 `toLocaleString()` , `toUTCString()` 등의 메소드를 사용하여 원하는 포맷으로 날짜와 시간을 표시할 수 있습니다.

## 참고

- [MDN web docs - Date](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [w3schools - JavaScript Date](https://www.w3schools.com/jsref/jsref_obj_date.asp)