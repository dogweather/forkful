---
title:                "Javascript: 미래 또는 과거의 날짜 계산하기"
simple_title:         "미래 또는 과거의 날짜 계산하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래 또는 과거로 계산해야 할 이유는 매우 다양합니다. 가장 흔한 이유로는 예약 시스템이나 이벤트 플래너와 같은 애플리케이션에서 사용될 수 있습니다. 사용자가 특정 날짜에 대한 정보를 얻을 수 있도록 도와주고, 언제나 원하는 미래 또는 과거의 날짜를 계산할 수 있도록 돕는 것입니다.

## 하르 투

미래 또는 과거의 날짜를 계산하는 방법은 간단합니다. 먼저, Javascript에서 날짜를 다루기 위해 내장된 Date 객체를 사용해야 합니다. Date 객체를 통해 현재 시간을 가져오고, 이를 바탕으로 미래 또는 과거의 날짜를 계산할 수 있습니다. 아래는 예제 코드와 함께 사용된 결과입니다.

```Javascript
// 현재 시간 생성
let today = new Date();

// 미래의 날짜 계산 (100일 후)
today.setDate(today.getDate()+100);
console.log(today); // 2020-09-26T14:39:43.659Z

// 과거의 날짜 계산 (1년 전)
today.setFullYear(today.getFullYear()-1);
console.log(today); // 2019-06-19T14:40:45.898Z
```

## 딥 다이브

날짜를 계산하는 것은 실제로 복잡한 작업이 아닙니다. 하지만 좀 더 깊이 들어가면, Javascript에서 날짜를 다루는 방식에 대해 좀 더 알 수 있습니다. 예를 들어, Date 객체의 메소드 중 하나인 `setDate()`은 날짜를 설정할 때 일부러 1을 빼주어야 하는 이유가 있습니다. 이는 1월부터 시작하는 인덱싱 방식 때문입니다.

또한, 미래 또는 과거의 날짜를 계산할 때 윤년을 고려해야 할 수도 있습니다. 이러한 작은 세부사항들을 알아두면, 더 정확하고 효율적인 날짜 계산을 할 수 있습니다.

## 봐도록 하기

- Date 객체에 대한 자세한 정보: https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date
- 윤년에 대한 이해: https://ko.wikipedia.org/wiki/%EC%9C%A4%EB%85%84