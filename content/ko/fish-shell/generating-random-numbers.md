---
title:                "Fish Shell: 랜덤 숫자 생성"
simple_title:         "랜덤 숫자 생성"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜
난수를 생성하는 것에 대해 관심이 있는 이유는 무엇일까요? 하지만 이 기술을 사용하면 다양한 목적으로 활용할 수 있습니다. 예를 들어, 게임에서 무작위 이벤트를 발생시키거나 보안 코드를 생성하는 등 다양하게 활용할 수 있습니다.

## 어떻게
난수를 생성하는 방법은 간단합니다. 다음 예제 코드를 통해 살펴봅시다.

```Fish Shell
# 랜덤한 정수 생성
set random_num (random 1 10)
echo $random_num
```

위 코드에서는 `random` 함수를 사용하여 범위 내에서 무작위 정수를 생성하고, `set` 명령어를 통해 변수에 저장합니다. 이렇게 생성된 난수는 `echo` 명령어를 통해 출력할 수 있습니다. 또한 다양한 난수 생성 함수들을 조합하여 더욱 다양한 결과를 얻을 수 있습니다.

```Fish Shell
# 랜덤한 소수 생성
set random_decimal (math tofloat (rand))
echo $random_decimal
```

위 코드는 `math` 함수와 `rand` 함수를 사용해서 0부터 1 사이의 무작위 소수를 생성하고, `tofloat` 함수를 통해 소수로 변환한 후 변수에 저장하고 출력하는 예제입니다.

## 딥 다이브
난수를 생성하는 방법에는 다양한 알고리즘이 존재합니다. 가장 일반적인 알고리즘은 `random` 함수를 사용하는 것이지만, 더욱 복잡한 난수 생성을 위해서는 다른 방법을 사용해야 합니다. 예를 들어, 시드(seed) 값을 설정하여 더 특정한 난수를 생성할 수 있습니다. 또한 난수 생성기의 안전성과 예측 불가능성을 보장하기 위해 암호학적으로 강화된 알고리즘을 사용할 수도 있습니다.

## 참고 자료
- [Fish Shell 공식 문서](https://fishshell.com/docs/current/)
- [Generating Random Numbers in Fish Shell](https://medium.com/@lampe/random-numbers-in-fish-shell-a8e398a8c175)
- [Secure Random Number Generation in Fish Shell](https://github.com/oh-my-fish/plugin-random)