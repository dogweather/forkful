---
title:                "Javascript: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜
두 개의 날짜를 비교하는 것이 중요한 이유는 우리가 시간에 대한 정확한 이해와 계획을 할 수 있기 때문입니다. 예를 들어, 우리는 날짜를 비교하여 과거의 데이터와 현재의 데이터를 분석하고 미래를 예측할 수 있습니다.

## 어떻게
날짜를 비교하는 것은 Javascript에서 아주 간단한 작업입니다. 먼저 비교하고 싶은 두 날짜를 각각 변수에 할당합니다.

```javascript
let firstDate = new Date("2021-01-01");
let secondDate = new Date("2021-03-15");
```

다음으로, 비교 연산자를 사용하여 두 날짜를 비교할 수 있습니다. 예를 들어, 첫 번째 날짜가 두 번째 날짜보다 이후인지 여부를 확인하고 싶다면 다음과 같이 코드를 작성할 수 있습니다.

```javascript
if (firstDate > secondDate){
  console.log("첫 번째 날짜가 두 번째 날짜보다 이후에 있습니다.");
} else if (firstDate < secondDate) {
  console.log("첫 번째 날짜가 두 번째 날짜보다 이전에 있습니다.");
} else {
  console.log("두 날짜가 동일합니다.");
}
```

출력 결과는 다음과 같이 나올 것입니다.

```
첫 번째 날짜가 두 번째 날짜보다 이전에 있습니다.
```

## 깊게 파보기
Javascript에서 날짜를 비교할 때, 우리가 사용하는 비교 연산자는 날짜 객체들 간의 "시간"을 비교하는 것입니다. 따라서 두 날짜가 동일한 "일"이지만 다른 "시간"을 갖는다면 여전히 다른 날짜로 간주될 수 있습니다. 이 점을 유의하여 날짜를 비교하는 것이 중요합니다.

## 더보기
- [MDN 날짜 객체](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools Javascript 비교 연산자](https://www.w3schools.com/js/js_comparisons.asp)