---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "TypeScript: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜
이 글은 TypeScript로 새로운 날짜를 계산하고 날짜를 더하거나 뺄 때 유용한 방법을 소개하기 위한 것입니다.

## 하는 법
우선, `Date` 클래스를 사용하여 새로운 날짜를 만들 수 있습니다. 그 다음, `setFullYear()` 함수를 사용하여 해당 날짜에 연도를 설정할 수 있습니다. 아래의 예시 코드를 참고해보세요.

```TypeScript
let date = new Date();
date.setFullYear(2022);
console.log(date);
```

위 코드를 실행하면 콘솔에는 현재 날짜에서 연도가 2022년인 새로운 날짜가 출력됩니다.

또한, 만약 현재 날짜에서 3년 후의 날짜를 계산하고 싶다면 `setFullYear()` 함수에 파라미터로 현재 연도에 3을 더해주면 됩니다.

```TypeScript
let date = new Date();
date.setFullYear(date.getFullYear() + 3);
console.log(date);
```

위 코드를 실행하면 콘솔에 현재 날짜에서 3년 후의 날짜가 출력됩니다.

## 깊이 파고들기
이외에도 `Date` 클래스는 다양한 메소드를 가지고 있어서 원하는 날짜를 계산하는 데에 유용하게 사용할 수 있습니다. 예를 들어, `setMonth()` 함수를 사용하면 해당 날짜의 달을 설정할 수 있고, `setDate()` 함수를 사용하면 해당 날짜의 일을 설정할 수 있습니다. 또한, `getDate()` 함수를 사용하면 현재 날짜의 일을 가져오는 것도 가능합니다. 이와 같이 `Date` 클래스의 다양한 함수를 활용하면 원하는 날짜를 쉽게 계산할 수 있습니다.

## 관련 링크
- [MDN - Date](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript 핸드북 - 날짜와 시간](https://typescript-kr.github.io/pages/Basic%20Types.html#%EB%82%A0%EC%A7%9C%EC%99%80-%EC%8B%9C%EA%B0%84)