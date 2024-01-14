---
title:                "TypeScript: 미래나 과거의 날짜 계산하기"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 것에 참여하는 이유는 다양합니다. 예를 들어, 애플리케이션에서 특정 날짜 이전에 알림을 설정하거나, 특정 날짜까지 이벤트를 계획하는 등 다양한 이유가 있을 수 있습니다.

## 어떻게

우선, TypeScript에서 날짜를 계산하기 위해서는 Date 객체를 사용해야 합니다. 이 객체는 TypeScript에 기본적으로 내장되어 있으며, 다음과 같이 사용할 수 있습니다.

```TypeScript
// 현재 날짜를 가져옴
let currentDate = new Date();

// 미래의 날짜를 계산함
let futureDate = new Date(currentDate.getFullYear(), currentDate.getMonth(), currentDate.getDate() + 7);

// 과거의 날짜를 계산함
let pastDate = new Date(currentDate.getFullYear(), currentDate.getMonth(), currentDate.getDate() - 7);
```

위 예시에서는 미래의 날짜를 계산하는 방법과 과거의 날짜를 계산하는 방법을 보여줍니다. 이 외에도 Date 객체를 이용하여 다양한 날짜 계산을 할 수 있습니다.

## 딥 다이브

Date 객체의 생성자와 메소드를 살펴보면, 이 객체를 이용하여 다양한 날짜 계산을 할 수 있습니다. 또한 날짜의 포맷을 바꿀 수 있는 toLocaleDateString() 메소드와 같은 유용한 기능도 제공됩니다.

하지만 주의해야 할 점은, Date 객체는 브라우저나 환경에 따라 다르게 동작할 수 있으며, 타임존이나 일광 절약 시간 등에 영향을 받을 수 있다는 것입니다. 때문에 정확한 계산을 위해서는 서드파티 라이브러리를 사용하는 것이 좋습니다.

## 더 알아보기

GitHub 날짜 라이브러리: https://github.com/date-fns/date-fns
Moment.js: https://momentjs.com/
date-fns: https://date-fns.org/