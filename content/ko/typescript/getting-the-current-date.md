---
title:    "TypeScript: 현재 날짜 가져오기"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## 왜

현재 날짜를 얻는 일은 프로그래밍에서 자주 발생하는 작업입니다. 이를 위해 개발자들은 다양한 메소드와 라이브러리를 사용합니다. 자주 사용되는 방법 중 하나인 TypeScript를 이용해 현재 날짜를 얻는 법을 살펴보겠습니다.

## 어떻게 해야 할까

```TypeScript
const currentDate = new Date();
```

위의 코드는 JavaScript에서 사용 가능한 방법 중 하나입니다. 이는 Date 클래스의 인스턴스를 만들어 현재 시간과 날짜를 저장한 변수를 반환합니다. TypeScript의 강력한 타입 시스템을 이용해 currentDate 변수의 타입을 지정하여 더욱 안전하게 코드를 작성할 수 있습니다.

```TypeScript
const currentDate: Date = new Date();
```

추가적으로, 날짜의 특정 부분만 얻고 싶을 때는 다음과 같은 메소드들을 사용할 수 있습니다.

```TypeScript
const date: number = currentDate.getDate(); // 현재 날짜 반환
const month: number = currentDate.getMonth(); // 현재 월 반환, 1월은 0부터 시작
const year: number = currentDate.getFullYear(); // 현재 년도 반환
const day: number = currentDate.getDay(); // 현재 요일 반환, 일요일은 0부터 시작
const hours: number = currentDate.getHours(); // 현재 시간 반환
const minutes: number = currentDate.getMinutes(); // 현재 분 반환
const seconds: number = currentDate.getSeconds(); // 현재 초 반환
const milliseconds: number = currentDate.getMilliseconds(); // 현재 밀리초 반환
```

## 딥 다이브

날짜와 관련된 이슈가 발생하기 쉬운 부분 중 하나는 시간대입니다. 프로그램이 실행되는 서로 다른 지역에서 현재 날짜를 얻기 위해서는 로컬 시간이 아닌 UTC 시간을 사용해야 합니다. 따라서 UTC 현재 날짜를 얻으려면 다음과 같이 코드를 작성할 수 있습니다.

```TypeScript
const currentDate = new Date();
const utcDate = new Date(Date.UTC(currentDate.getFullYear(), currentDate.getMonth(), currentDate.getDate(), currentDate.getHours(), currentDate.getMinutes(), currentDate.getSeconds(), currentDate.getMilliseconds()));
```

이제 `utcDate` 변수에는 UTC 기준의 현재 날짜가 저장되어 있습니다. 

## 더 알아보기

추가적인 정보와 다른 방법으로 현재 날짜를 얻는 방법에 대해 알아보려면 아래의 링크들을 참고해보세요.

- [Date 클래스 공식 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [Luxon](https://moment.github.io/luxon/) 

## 참고

- [TypeScript 공식 사이트](https://www.typescriptlang.org/)
- [Node.js 공식 사이트](https://nodejs.org/ko/)