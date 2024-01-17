---
title:                "두 날짜 비교"
html_title:           "TypeScript: 두 날짜 비교"
simple_title:         "두 날짜 비교"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 할까요?
날짜 비교는 두 날짜가 일치하는지 또는 어느 날짜가 더 이전 또는 이후인지 판단하는 것을 말합니다. 프로그래머들은 이를 실행하는 이유는 다양합니다. 예를 들어, 날짜 비교를 통해 기간이 지났는지 여부를 알 수 있습니다. 또는 특정 날짜 이후에 어떤 작업을 수행해야 하는 경우에도 날짜 비교를 사용할 수 있습니다.

## 방법:
```TypeScript
const date1 = new Date('2021-01-01');
const date2 = new Date('2021-06-01');
const result = date1 < date2;
console.log(result); // true
```
위의 예시는 두 날짜를 비교하여 첫 번째 날짜가 두 번째 날짜보다 이전인지를 판단합니다. ```Date``` 타입은 ```<```와 ```>``` 연산자를 지원하므로 날짜를 간단하게 비교할 수 있습니다. 비교 결과를 콘솔에 출력하면 ```true```가 나오는 것을 확인할 수 있습니다.

## 더 깊이 들어가보면:
날짜 비교는 매우 일반적인 작업이지만, 과거에는 더 어려웠습니다. 예를 들어, 자바스크립트에서는 날짜를 비교하기 전에 각 날짜의 일자를 추출하여 비교해야 했습니다. 하지만 TypeScript에서는 ```Date``` 타입이 제공되므로 이러한 추가 작업 없이 바로 비교할 수 있습니다.

또한 TypeScript에서는 두 개의 날짜를 동시에 비교하는 방법도 제공합니다. 예를 들어, 두 날짜가 정확히 일치하는지 확인하려면 ```===``` 연산자를 사용하면 됩니다.

다른 언어에서는 비교 연산자를 사용하지 않고 Date 객체의 메소드를 사용하여 날짜를 비교하는 경우도 있습니다. 이 방법은 상대적으로 더 복잡하지만 날짜의 속성을 자세히 알고 싶은 경우에는 유용할 수 있습니다.

## 관련 정보 보기:
- [JavaScript Date 객체 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript 공식 문서](https://www.typescriptlang.org/docs/)
- [타입스크립트로 배우는 프로그래밍 기초](https://poiemaweb.com/typescript-introduction)