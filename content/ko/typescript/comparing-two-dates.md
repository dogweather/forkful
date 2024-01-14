---
title:    "TypeScript: 두 날짜 비교"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

"## 왜"

두 날짜를 비교하는 것이 왜 중요한지 궁금하신가요? TypeScript를 사용하는 경우, 날짜 비교는 두 날짜가 같은지 또는 어느 날짜가 빠른지 등을 확인하는 데 유용합니다.

"## 어떻게"

두 날짜를 비교하는 방법은 간단합니다. 먼저, 두 날짜 객체를 생성하고 `>` 나 `<` 연산자를 사용하여 비교할 수 있습니다. 또는 `getTime()` 함수를 사용하여 날짜를 숫자형으로 변환한 다음, 숫자 연산으로도 비교할 수 있습니다.

```TypeScript
let date1 = new Date(2021, 5, 25);
let date2 = new Date(2021, 3, 12);

if (date1 > date2) {
    console.log(date1 + " is later than " + date2);
}

let time1 = date1.getTime();
let time2 = date2.getTime();

if (time1 < time2) {
    console.log(date1 + " is earlier than " + date2);
}
```

위의 예시 코드를 실행하면 다음과 같은 결과가 출력됩니다.

```
Wed Jun 23 2021 00:00:00 GMT+0900 (Korean Standard Time) is later than Fri Apr 02 2021 00:00:00 GMT+0900 (Korean Standard Time)
Wed Jun 23 2021 00:00:00 GMT+0900 (Korean Standard Time) is earlier than Thu Mar 12 2021 00:00:00 GMT+0900 (Korean Standard Time)
```

"## 깊은 살펴보기"

날짜를 비교할 때, 주의해야 할 점이 있습니다. 바로 `getMonth()` 함수를 사용할 때입니다. 이 함수는 0부터 시작하기 때문에, 6월을 나타내는 `getMonth()` 값은 `5`가 됩니다. 따라서, 실수를 방지하기 위해 직접 `Date` 객체를 생성할 때, `getMonth()` 값을 명시적으로 1씩 빼주는 것이 좋습니다.

또한, 두 날짜의 밀리초 값이 같아도 객체의 타입이 다를 경우 비교 결과가 다를 수 있습니다. 따라서, 객체의 타입을 동일하게 맞춰주는 것도 중요합니다.

"## See Also"

- [MDN web docs: Date 객체](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript 공식 문서](https://www.typescriptlang.org/docs)