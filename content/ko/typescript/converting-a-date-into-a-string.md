---
title:                "날짜를 문자열로 변환하기"
html_title:           "Arduino: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇이고 왜합니까?

날짜를 문자열로 변환하는 것은 일반적인 프로그래밍 작업입니다. 이는 날짜를 사람이 읽을 수 있는 형식으로 표시하거나, 코드에서 다루기 더 쉬운 데이터 형식으로 변경하기 위해 필요합니다.

## 어떻게:

TypeScript에서 날짜를 문자열로 변환하는 방법은 여러가지가 있습니다. 아래는 그 중 일부를 보여줍니다:

```TypeScript
let date: Date = new Date();
console.log(date.toString()); // "Wed Dec 11 2019 12:09:40 GMT+0530 (India Standard Time)"
console.log(date.toISOString()); // "2019-12-11T06:39:40.929Z"
```

위 코드에서, `toString()` 메서드는 우리가 일반적으로 화면에서 볼 수 있는 형식으로 날짜를 문자열로 변환합니다. 반면에 `toISOString()` 메서드는 ISO 8601 날짜 형식으로 변환합니다.

## 깊이 들여다보기:

날짜를 문자열로 변환하는 방법이 여러 가지가 있는데, 이는 프로그래밍 언어들의 역사적 맥락에서 비롯되었습니다. 예를 들어, 초창기의 C 언어에는 날짜 형식을 포현하기 위한 공식적인 방법이 없었습니다. 그래서 개발자들은 초, 분, 일, 월, 연도 등을 각각의 값으로 저장한 배열을 사용했습니다. 이것이 문제가 될 수 있는 일부 상황을 해결하기 위해, 차후에 많은 언어들이 날짜를 문자열로 변환하는 메서드를 제공하기 시작했습니다.

TypeScript에서는 `toString()`, `toISOString()`, `toJSON()`, `toLocaleString()`, `toLocaleDateString()`, `toLocaleTimeString()` 등 다양한 메서드를 제공합니다. 이 메서드들은 모두 특정 상황에서 사용하기에 가장 적합한 날짜 형식을 문자열로 반환합니다.

## 참조:

다음 링크에서 날짜를 문자열로 변환하는 데 관한 추가 정보를 찾을 수 있습니다:
- [Mozilla JavaScript Date Documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Microsoft TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/basic-types.html#more-on-strings)