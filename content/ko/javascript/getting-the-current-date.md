---
title:                "현재 날짜 가져오기"
html_title:           "Javascript: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

날짜를 알아내는 것이 왜 중요한지에 대해 생각해보세요. 생일을 기억하는 것부터 시작해서, 다른 날짜 정보를 추적하거나 시간을 계산하는 등의 다양한 상황에서 날짜 정보가 필요할 수 있습니다.

## 코딩하는 법

```Javascript
// 새로운 Date 객체를 생성합니다.
let currentDate = new Date();

// 날짜 정보를 추출할 수 있습니다.
let day = currentDate.getDate();
let month = currentDate.getMonth() + 1; // 0부터 시작하므로 1을 더해줍니다.
let year = currentDate.getFullYear();

// 결과를 출력합니다.
console.log(`${month}/${day}/${year}`); // 예: 8/10/2021
```

```Javascript
// 한 줄로 날짜 정보를 추출하는 것도 가능합니다.
let currentDate = new Date();
console.log(currentDate.toLocaleDateString()); // 예: 8/10/2021
```

## 깊이 파헤치기

이제 Date 객체를 생성하고 날짜 정보를 추출하는 방법을 알게 되었습니다. 하지만 이 날짜 정보는 우리가 사용하는 시간대를 기준으로 반환됩니다. 예를 들어, 한국에서는 날짜 정보가 한국 시간대를 기준으로 반환됩니다. 따라서 국제적으로 사용되는 시간 정보를 사용할 때에는 조심해야 합니다. 자세한 내용은 [공식 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)에서 확인할 수 있습니다.

## 더 알아보기

* [Date 객체 공식 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
* [Date 객체 관련 팁](https://www.digitalocean.com/community/tutorials/understanding-date-and-time-in-javascript)
* [Date 객체를 사용하는 방법](https://www.w3schools.com/js/js_date_methods.asp)
* [날짜 형식을 설정하는 방법](https://www.geeksforgeeks.org/how-to-convert-date-to-another-timezone-in-javascript/)