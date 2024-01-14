---
title:    "TypeScript: 날짜를 문자열로 변환하기"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜
날짜를 문자열로 변환하는 작업은 프로그래밍에서 매우 흔하게 사용되는 기능입니다. 날짜를 문자열로 바꾸는 이유는 다양합니다. 예를 들어, 데이터베이스에 날짜 형식으로 저장된 값을 웹 사이트에서 보여줄 때, 사용자가 입력한 날짜를 데이터베이스와 비교할 때, 혹은 간단한 계산을 수행할 때 등 많은 상황에서 날짜를 문자열로 변환합니다.

## 방법
날짜를 문자열로 변환하는 방법은 간단합니다. TypeScript에서는 Date 객체와 다양한 내장 메소드를 활용하여 날짜를 간편하게 문자열로 변환할 수 있습니다. 아래의 예제 코드를 참고해보세요.

```TypeScript
let today = new Date(); // 현재 날짜 정보를 가진 Date 객체를 생성합니다.
let dateString = today.toLocaleDateString(); // Date 객체의 toLocaleDateString 메소드를 호출하여 날짜를 문자열로 변환합니다.
console.log(dateString); // "2021-10-08"과 같은 표준 형식으로 출력됩니다.
```

위에서 사용한 toLocaleDateString 메소드는 내부적으로 사용자의 지역 설정에 따라 다른 문자열 형식을 반환합니다. 따라서 필요에 따라 다양한 형식의 문자열로 변환할 수 있습니다.

```TypeScript
console.log(today.toLocaleDateString('en-US')); // "10/8/2021"
console.log(today.toLocaleDateString('ko-KR')); // "2021. 10. 8."
```

만약 날짜와 시간을 모두 포함한 문자열을 반환하고 싶다면 toLocalString 메소드를 사용할 수 있습니다.

```TypeScript
let today = new Date();
let dateTimeString = today.toLocaleString(); // Date 객체의 toLocaleString 메소드를 호출하여 날짜와 시간을 문자열로 변환합니다.
console.log(dateTimeString); // "10/8/2021, 12:00:00 AM"과 같은 형식으로 출력됩니다.
```

## 깊이 파고들기
위에서는 날짜를 문자열로 변환하는 가장 기본적인 방법을 살펴보았습니다. 하지만 우리는 더 많은 선택지를 가지고 있습니다. Date 객체에는 날짜, 시간, 혹은 둘 다를 포함한 값들을 반환하는 다양한 메소드가 있고, 또 JavaScript에서는 Moment.js와 같은 라이브러리를 사용하여 더 다양한 포맷의 날짜 문자열을 만들 수 있습니다.

더 깊이 들어가기 전에, 날짜와 문자열을 자유자재로 다루기 위해서는 기본적인 날짜와 시간에 대한 지식이 필요합니다. 따라서 날짜와 시간 관련해서 공부해보는 것을 추천합니다.

## 관련 정보
- [TypeScript Date 객체 문서](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html)
- [JavaScript Moment.js 라이브러리](https://momentjs.com/)
- [날짜와 시간 관련 자습서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)