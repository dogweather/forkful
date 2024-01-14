---
title:    "TypeScript: 두 날짜 비교하기"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 왜: 두 날짜를 비교하는 것이 필요한 이유
작성자님의 코딩 여정에서 두 날짜를 서로 비교하는 경우가 있을 것입니다. 이 글에서는 TypeScript를 사용하여 다양한 방법으로 날짜를 비교하는 방법을 배우게 될 것입니다.

## 어떻게: TypeScript를 사용하여 두 날짜 비교하기
```TypeScript
// 두 날짜 변수 선언
let date1: Date = new Date('2021/01/01');
let date2: Date = new Date('2021/01/02');

// 두 날짜 비교
if(date1.getTime() === date2.getTime()) {
    console.log('두 날짜는 같습니다.');
} else if(date1.getTime() > date2.getTime()) {
    console.log('date1이 date2보다 이후 날짜입니다.');
} else {
    console.log('date2가 date1보다 이후 날짜입니다.');
}

// 출력 결과: date2가 date1보다 이후 날짜입니다.
```

위의 예시는 `Date` 객체의 `getTime()` 메서드를 사용하여 두 날짜를 밀리초로 바꾼 뒤 비교하는 방법을 보여줍니다. 다른 방법으로는 `>` 이나 `<` 연산자를 사용하여 비교하는 방법이 있습니다. `Date` 객체에 대한 자세한 정보는 [이 공식 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)를 참고하시기 바랍니다.

## 딥 다이브: 두 날짜 비교에 대해 더 알아보기
날짜를 비교할 때에는 날짜 형식이 같은지 확인하는 것이 중요합니다. 예를 들어, `'2021/10/15'`와 `'10/15/2021'`는 같은 날짜를 나타내지만 서로 다른 형식이기 때문에 비교할 수 없습니다. 따라서 먼저 두 날짜의 형식을 일치시켜주는 작업이 필요할 수 있습니다.

또한 날짜를 비교할 때에는 주의해야 할 점이 있습니다. 아래 코드를 보시면 어떤 점이 문제가 될 수 있는지 생각해보세요.

```TypeScript
// 두 날짜 변수 선언
let date1: Date = new Date('2021-01-01');
let date2: Date = new Date('2021/01/01');

// 두 날짜 비교
console.log(date1 === date2);

// 출력 결과: false
```

위의 예시에서는 두 날짜를 다른 형식으로 입력해주고 있지만, 결과는 `false`가 나오게 됩니다. 이는 TypeScript에서 `===` 연산자를 사용하여 비교할 때, 두 객체의 주소값을 비교하기 때문입니다. 따라서 날짜를 비교할 때에는 `getTime()` 메서드를 사용하거나, `getDate()`, `getMonth()`, `getFullYear()` 메서드를 사용하여 날짜를 숫자로 변환하고 비교하는 것이 중요합니다.

## 이 외에도 참고할 만한 자료들
- [TypeScript 공식 문서](https://www.typescriptlang.org/docs/)
- [MDN 웹 문서](https://developer.mozilla.org/ko/)
- [날짜 형식에 대한 더 많은 정보](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date/toISOString)