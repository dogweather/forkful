---
title:                "임의의 숫자 생성하기"
html_title:           "Elixir: 임의의 숫자 생성하기"
simple_title:         "임의의 숫자 생성하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 그렇게 하는가?

무작위 숫자 생성은 미리 정해진 규칙 없이 숫자를 생성하는 프로세스입니다. 이를 통해 프로그래머는 다양성을 향상시키고 예측을 어렵게 하며, 테스트 용도로도 사용합니다.

## 사용 방법 :

TypeScript에서 무작위 숫자를 생성하는 것은 매우 간단합니다.

```TypeScript
function getRandomNumber(min: number, max: number) {
    return Math.floor(Math.random() * (max - min + 1)) + min;
}

console.log(getRandomNumber(1, 100)); // 예: 42
```
이 코드는 'min'과 'max' 사이의 무작위 정수를 반환합니다.

## 심화 학습 :

1. **역사적 상황:** 무작위 숫자 생성은 컴퓨터 과학의 오래된 주제입니다. 여러 알고리즘이 개발되었지만, 파이썬은 'Mersenne Twister' 알고리즘을 사용합니다.

2. **대안:** 더 복잡한 대안은 보안 영역에서 사용되는 '암호화학적으로 안전한 난수'를 생성하는 것입니다. 이것은 안전한 통신과 암호화에서 중요합니다.

3. **구현 세부 사항:** `Math.random()` 함수는 `[0,1)` 범위의 무작위 실수를 생성합니다. 이를 기반으로 우리는 원하는 범위의 정수를 생성할 수 있습니다.

## 관련 자료 :

- MDN에 있는 [`Math.random()`](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Math/random)에 대한 설명.
- StackOverFlow의 질문, [How to generate a random integer number from within a range](https://stackoverflow.com/questions/1527803/generating-random-whole-numbers-in-javascript-in-a-specific-range)
- C언어에서의 [Random Number Generation](https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm) 방법 설명.