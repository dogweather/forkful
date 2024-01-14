---
title:    "TypeScript: 랜덤 숫자 생성하기"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜
랜덤 숫자를 생성하는 것이 왜 중요한지 생각해 보면, 여러분의 프로그램을 보다 다양하게 만들기 위해 필수적입니다. 예를 들어, 게임을 만들 때 랜덤 숫자를 사용해서 적들이 어디에 나타날지, 아이템이 어디에 놓여질지 등을 결정할 수 있습니다. 랜덤 숫자를 생성할 줄 안다면, 더욱 흥미로운 프로그램을 만들어볼 수 있을 것입니다.

## 방법
먼저, TypeScript에서 랜덤 숫자를 생성하기 위해서는 Math 라이브러리를 사용해야 합니다. 이 라이브러리에는 여러 수학적 함수들이 내장되어 있어 매우 편리합니다. 아래는 TypeScript로 랜덤 숫자를 생성하는 간단한 예제입니다.

```TypeScript
// 랜덤 숫자를 생성하는 함수
function generateRandomNumber() {
  // 0에서 10 사이의 랜덤 숫자를 생성
  const randomNumber = Math.floor(Math.random() * 10);

  // 생성된 숫자를 출력
  console.log(randomNumber);
}

// 함수 호출
generateRandomNumber();
```

위 코드를 실행하면, 콘솔에서 0에서 10 사이의 랜덤 숫자를 출력할 수 있습니다. 이제 이 함수를 이용해서 다양한 곳에서 랜덤 숫자를 사용할 수 있게 되었습니다.

## 깊이 파고들기
랜덤 숫자를 생성하는데에는 여러 가지 방식이 있습니다. 위 예제에서는 Math 라이브러리의 `Math.random()` 메소드를 사용하였지만, 다른 방법도 있습니다. 예를 들어, `crypto` 라이브러리를 사용하여 보다 안전한 랜덤 숫자를 생성할 수 있습니다.

또한, 랜덤 숫자를 사용하는 알고리즘에 따라 결과값이 달라질 수 있습니다. 따라서, 여러분이 원하는 방식과 결과에 따라 적절한 랜덤 숫자 생성 방법을 선택해야 합니다.

## 참고 자료
- [Math 라이브러리 | MDN](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Math)
- [crypto 라이브러리 | Node.js Documentation](https://nodejs.org/api/crypto.html)
- [랜덤 숫자 생성 알고리즘 | Wikipedia](https://en.wikipedia.org/wiki/Random_number_generation_algorithm)