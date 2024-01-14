---
title:                "TypeScript: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜: 현재 날짜를 가져오는 이유

날짜는 우리 일상에서 매우 중요한 역할을 합니다. 우리는 구체적으로 어느 날짜인지에 따라 일정을 계획하고, 생일을 축하하며, 회계 업무를 처리합니다. 때문에, TypeScript를 사용하여 현재 날짜를 가져오는 작업은 매우 일상적이고 유용합니다.

## 어떻게: TypeScript를 사용하여 현재 날짜 가져오기

TypeScript를 사용하여 현재 날짜를 쉽게 가져올 수 있습니다. 다음과 같은 코드를 사용하여 현재 날짜를 가져올 수 있습니다.

```TypeScript
const currentDate = new Date();
console.log(currentDate);
```

위의 코드를 실행하면, 다음과 같은 결과를 볼 수 있습니다.

```
2021-02-14T21:00:00.000Z
```

여기서 `T21:00:00.000Z`는 현재 날짜와 시간을 나타냅니다. 이와 같은 방식으로, 우리는 `getDate()`를 사용하여 현재 날짜를 정확하게 가져올 수 있습니다. 예를 들어:

```TypeScript
const currentDate = new Date();
const currentDay = currentDate.getDate();
console.log(currentDay);
```

위의 코드를 실행하면 현재 일자가 나타납니다. 이렇게 간단하게도 TypeScript를 사용하여 현재 날짜를 가져올 수 있습니다.

## 딥 다이브: 현재 날짜 가져오는 더 깊은 정보

이전에 우리는 `getDate()`를 사용하여 현재 일자를 가져오는 방법을 살펴보았습니다. 하지만 실제로는, 우리는 `getFullYear()`, `getMonth()`, `getHours()`와 같은 다른 메서드도 사용할 수 있습니다. 이러한 메서드를 사용하면 날짜와 시간을 더 정확하게 가져올 수 있습니다. 또한, 특정 날짜를 지정하여 해당 날짜의 정보를 가져올 수도 있습니다.

이 외에도 많은 방법을 사용하여 현재 날짜와 관련된 작업을 할 수 있습니다. TypeScript 공식 문서에서 더 많은 정보를 얻을 수 있으며, 실제 예제를 살펴보면서 더 깊이 있는 지식을 습득할 수 있습니다.

## 더 알아보기

- [TypeScript 공식 문서](https://www.typescriptlang.org/docs)
- [TypeScript로 현재 날짜와 관련된 기능 다루기](https://www.techiediaries.com/typescript-get-todays-date/)
- [TypeScript로 현재 시간 가져오기](https://attacomsian.com/blog/javascript-get-time-date)