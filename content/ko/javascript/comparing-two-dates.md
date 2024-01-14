---
title:                "Javascript: 두 날짜 비교하기"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

날짜를 비교하는 것에 대해 사람들이 참여하게 되는 이유는 무엇인가요?

날짜를 비교하는 것은 자바스크립트 프로그래밍에서 매우 중요한 부분입니다. 우리는 우리의 코드가 올바른 날짜를 처리하는지 확인해야 하기 때문입니다. 따라서 날짜를 비교하는 기술은 매우 유용합니다. 그것은 우리가 우리의 코드를 더욱 효율적이고 정확하게 만들어 줍니다.

## 사용 방법

`Date` 객체는 날짜를 저장하고 비교하는 데 사용됩니다. 우리는 두 개의 `Date` 객체를 만들고 `getTime` 메소드를 사용하여 각 날짜의 시간 값을 가져올 수 있습니다. 이 시간 값들은 밀리 초 단위로 표현됩니다. 그런 다음 우리는 각 시간 값을 비교하는 데 사용할 수 있는 조건문을 작성할 수 있습니다.

아래는 두 개의 날짜를 비교하는 예시 코드입니다.

``` Javascript
const date1 = new Date('2021-05-10');
const date2 = new Date('2021-05-12');

if (date1.getTime() === date2.getTime()) {
  console.log('두 날짜가 같습니다.');
} else if (date1.getTime() < date2.getTime()) {
  console.log('첫 번째 날짜가 두 번째 날짜보다 이전입니다.');
} else {
  console.log('첫 번째 날짜가 두 번째 날짜보다 이후입니다.');
}
```

위 코드의 출력은 다음과 같이 나타납니다.

``` Javascript
첫 번째 날짜가 두 번째 날짜보다 이전입니다.
```

## 깊게 알아보기

우리는 위의 코드에서 `getTime` 메소드를 사용하여 각 날짜의 시간 값을 얻지만, 이것은 어떻게 동작하는지 실제로 이해해야 합니다. `Date` 객체는 1970년 1월 1일 00:00:00 UTC부터 지정된 날짜/시간까지의 밀리 초 값을 가질 수 있습니다. 1초는 1000밀리 초이므로, 이 값은 지정된 날짜/시간에서의 과거의 밀리 초 값을 나타냅니다. 따라서 `getTime` 메소드는 현재 날짜/시간과 지정된 날짜/시간 간의 밀리 초 차이를 반환하는 것입니다.

이러한 깊은 이해는 날짜 비교 기술을 더욱 유용하게 만들어 줍니다. 여러분은 더 복잡한 조건문을 작성하여 필요한 비교 기능을 구현할 수 있습니다.

## 또 다른 정보

- [MDN Web Docs - Date 객체](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [MDN Web Docs - getTime() 메소드](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date/getTime)