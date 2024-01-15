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

## 왜
비교하는 두 날짜를 비교하는 것이 중요한 이유는, 날짜는 우리의 일상 생활과 매우 밀접한 관련이 있기 때문입니다. 우리는 특정 날짜에 어떤 이벤트가 발생했는지 파악하고, 날짜를 통해 예정된 일정을 확인하며, 날짜를 이용해 생일을 축하합니다. 자바스크립트에서 날짜를 비교하는 것은 우리의 일상에서 매우 유용하게 사용되기 때문에 중요합니다.

## 어떻게 비교할까
```Javascript
const date1 = new Date('2020-10-01');
const date2 = new Date('2020-09-30');

if (date1 > date2) {
  console.log(date1 + ' is later than ' + date2);
} else if (date1 < date2) {
  console.log(date2 + ' is later than ' + date1);
} else {
  console.log('The dates are the same');
}

// Output: Wed Sep 30 2020 00:00:00 GMT+0900 (Korean Standard Time) is later than Thu Oct 01 2020 00:00:00 GMT+0900 (Korean Standard Time)
```
위의 코드는 두 날짜를 비교하는 간단한 예시입니다. 우선, `new Date()`를 통해 두 개의 날짜 객체를 생성해줍니다. 그리고 `if`문을 사용하여 두 날짜를 비교하고, `console.log()`를 통해 결과를 출력해줍니다.

또 다른 방법으로는 `getTime()` 메소드를 사용해 날짜를 밀리초로 변환하고, 비교하는 방법도 있습니다.

## 자세히 알아보기
JavaScript에서 날짜를 비교할 때에는 일반적으로 다음과 같은 방식으로 합니다.
- `>` : 왼쪽 날짜가 더 나중인지 확인
- `<` : 오른쪽 날짜가 더 나중인지 확인
- `==` : 두 날짜가 같은지 확인

하지만 두 날짜가 같은지를 `==`으로 확인하는 것은 매우 위험합니다. 왜냐하면 자바스크립트에서는 객체를 비교할 때 `==`를 사용하면 객체의 내부 값이 아닌 참조값을 비교하기 때문입니다. 따라서 `==` 대신 `===`를 사용하여 값을 비교하는 것이 더 좋습니다.

또한 `getTime()` 메소드를 사용하면 두 날짜 간의 차이를 밀리초로 구할 수 있습니다. 이를 이용하여 날짜 간격을 계산할 수 있습니다.

## 참고자료
- [MDN: Comparison operators](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Operators/Comparison_Operators)
- [w3schools: JavaScript Date Comparison](https://www.w3schools.com/js/js_date_comparisons.asp)
- [GeeksforGeeks: Comparing dates using JavaScript](https://www.geeksforgeeks.org/comparing-dates-using-javascript/)