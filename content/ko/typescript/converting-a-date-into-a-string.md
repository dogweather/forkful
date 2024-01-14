---
title:                "TypeScript: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Korean Translation:

## 왜

날짜를 문자열로 변환하는 이유를 설명합니다.

날짜를 문자열로 변환하는 작업은 종종 프로그램에서 필요한 작업입니다. 예를 들어, 사용자 인터페이스를 업데이트할 때 또는 데이터베이스에 저장된 날짜를 사용해 새로운 날짜를 생성할 때 필요합니다.

## 방법

먼저, JavaScript에서 날짜를 문자열로 변환하는 방법에 대해 간단히 알아보겠습니다. 그런 다음 TypeScript에서 이를 어떻게 구현하는지에 대해 살펴보겠습니다.

### JavaScript에서 날짜를 문자열로 변환하기

우선 `Date` 객체를 생성하고 `toString()` 메소드를 사용하여 해당 객체를 문자열로 변환합니다.

```JavaScript
const date = new Date();
date.toString(); // 문자열로 변환된 날짜: "Mon Oct 18 2021 08:00:00 GMT+0800 (Singapore Standard Time)"
```

하지만 이렇게 변환된 문자열은 날짜와 시간 모두를 포함하기 때문에 원하는 포맷이 아닐 수 있습니다. 이럴 때 `Intl.DateTimeFormat` 객체를 사용하면 원하는 포맷으로 날짜를 표시할 수 있습니다.

```JavaScript
const date = new Date();
const options = {
  weekday: 'short',
  year: 'numeric',
  month: 'short',
  day: 'numeric',
  timeZone: 'UTC'
};
const formatter = new Intl.DateTimeFormat('en-US', options);
formatter.format(date); // "Mon, Oct 18, 2021"
```

### TypeScript에서 날짜를 문자열로 변환하기

TypeScript에서 날짜를 문자열로 변환하는 방법은 JavaScript와 동일합니다. 다만, TypeScript는 정적으로 타입이 지정되기 때문에 `formatter.format()` 메소드의 인수를 올바른 타입으로 지정해주어야 합니다.

```TypeScript
const date: Date = new Date();
const options: Intl.DateTimeFormatOptions = {
  weekday: 'short',
  year: 'numeric',
  month: 'short',
  day: 'numeric',
  timeZone: 'UTC'
};
const formatter: Intl.DateTimeFormat = new Intl.DateTimeFormat('en-US', options);
formatter.format(date); // "Mon, Oct 18, 2021"
```

## 깊이 파보기

날짜를 문자열로 변환하는 내부 동작에 대해 더 알아보겠습니다. 날짜 객체는 컴퓨터 내부적으로 숫자로 저장되며, 이는 유닉스 시간 (Unix time) 이라고 불립니다. 1970년 1월 1일 자정을 기준으로 경과된 밀리초의 숫자를 나타냅니다. 이 숫자는 `Date` 생성자의 인수로 전달되어 날짜 객체를 생성하며, `toString()` 메소드에서는 이 숫자를 다시 날짜와 시간 형식으로 변환합니다.

## 참고 문서

- [MDN: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [MDN: Intl.DateTimeFormat](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript Docs: Date](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#using-dates)

## 참조

- [날짜 객체](https://ko.wikipedia.org/wiki/%EB%82%A0%EC%A7%9C_%EA%B0%9D%EC%B2%B4)
- [유닉스 시간](https://ko.wikipedia.org/wiki/%EC%9C%A0%EB%8B%89%EC%8A%A4_%EC%8B%9C%EA%B0%84)