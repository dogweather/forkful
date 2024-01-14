---
title:                "Javascript: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 왜

랜덤한 숫자를 생성하는 것에 참여하는 이유는 다양합니다. 몇 가지 예를 들어보면, 게임에서 랜덤하게 상황을 만들거나 보안 측면에서 패스워드 생성을 위한 무작위 숫자를 사용하는 등의 이유가 있을 수 있습니다.

## 하는 법

```Javascript
// 1부터 10까지의 랜덤한 숫자 생성
let randomNum = Math.floor(Math.random() * 10) + 1;
console.log(randomNum);
// 출력 결과, 예: 5

// 1부터 100까지의 랜덤한 숫자 생성
let randomNum = Math.floor(Math.random() * 100) + 1;
console.log(randomNum);
// 출력 결과, 예: 75

// 배열 안에서 랜덤한 아이템 선택
let array = [1, 2, 3, 4, 5];
let randomItem = array[Math.floor(Math.random() * array.length)];
console.log(randomItem);
// 출력 결과, 예: 3
```

위의 코드 예시에서는 `Math.random()` 메소드를 사용하여 랜덤한 숫자를 생성합니다. 이 메소드는 0부터 1 사이의 무작위 소수를 반환합니다. 따라서 범위를 지정하려면 `Math.floor()` 메소드를 이용해 소수 부분을 버린 다음, 곱하기 연산과 더하기 연산을 통해 범위를 지정할 수 있습니다. 또한 배열에서 랜덤한 아이템을 선택하려면 `Math.floor()` 메소드의 인자로 배열의 길이를 넣어준 다음, `array` 배열에 `randomItem` 변수를 할당하는 방식으로 구현할 수 있습니다.

## 깊이 알아보기

랜덤한 숫자를 생성하는데 사용되는 `Math.random()` 메소드는 실제로 무작위한 숫자인가요? 많은 사람들이 이 질문에 의문을 가지고 있습니다. 하지만 이 메소드는 컴퓨터 내부에서 생성된 시드 값을 기반으로 구현되어 있어서 매번 실행 시 동일한 결과를 보장할 수 있습니다. 따라서 완전히 무작위한 숫자는 아니지만 일반적인 경우에는 충분히 무작위한 숫자를 생성하는 데 사용할 수 있습니다.

## 이어서 보기

- [JavaScript Math.random() Documentation](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Randomness in JavaScript](https://dev.to/niorad/randomness-in-javascript-1jb0)
- [Using Math.random() to Generate Random Numbers in JavaScript](https://www.kirupa.com/html5/random_numbers_js.htm)