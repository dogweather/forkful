---
title:    "TypeScript: 미래나 과거의 날짜 계산하기"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 것에 대해 무엇이 궁금하신가요? 그렇다면, TypeScript를 사용하여 날짜를 계산하는 방법을 알아보는 것은 어떨까요? 이 작업을 통해 프로그램에서 날짜와 시간을 다루는 데 더욱 효과적으로 할 수 있습니다.

## 방법

더 많은 예시를 보고 싶다면, 아래 예시 코드 블록을 참고해주세요. 

```TypeScript
// 현재 날짜를 가져옵니다.
const today: Date = new Date();

// 7일 후의 날짜 계산
const futureDate: Date = new Date(today.setDate(today.getDate() + 7));

// 3달 전의 날짜 계산
const pastDate: Date = new Date(today.setMonth(today.getMonth() - 3));

// 콘솔에 결과 출력
console.log(`7일 후의 날짜: ${futureDate}`);
console.log(`3달 전의 날짜: ${pastDate}`);
```

위 코드는 현재 날짜를 기준으로 7일 후와 3달 전의 날짜를 계산하는 예시입니다. `setDate`와 `setMonth` 함수를 사용하여 간단하게 날짜를 계산할 수 있습니다. 이렇게 계산된 날짜는 `Date` 객체로 저장되어 프로그램에서 유용하게 활용할 수 있습니다.

## 심층 분석

코드 예시로 보여준 것처럼, JavaScript의 `Date` 객체를 사용하여 날짜를 계산할 수 있습니다. 이 객체는 많은 기능과 함수를 제공하며, 이를 활용하면 더욱 다양한 방법으로 날짜를 다룰 수 있습니다. 특히, 원하는 형식의 날짜를 출력하고 싶다면 `toLocaleString` 함수를 사용하면 됩니다.

## 참고 자료

- [Typescript - Date 오브젝트 계산하기](https://ujuc.github.io/2019/07/05/Typescript-Date-%EC%98%A4%EB%B8%8C%EC%A0%9D%ED%8A%B8-%EA%B3%84%EC%82%B0%ED%95%98%EA%B8%B0/)
- [MDN - Date 객체](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Typescript - Date 사용하기](https://gabii.tistory.com/entry/Typescript-Date-%EC%82%AC%EC%9A%A9%ED%95%98%EA%B8%B0)