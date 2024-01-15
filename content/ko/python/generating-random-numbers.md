---
title:                "난수 생성하기"
html_title:           "Python: 난수 생성하기"
simple_title:         "난수 생성하기"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜
왜 사람들이 무작위 숫자를 생성하는 일에 참여할까요? 무작위로 생성된 숫자는 많은 컴퓨터 프로그램에서 필수적인 기능으로 사용되기 때문입니다.

## 방법
아래의 코드 블록에는 무작위 숫자를 생성하는 다양한 예제와 그에 대한 출력 결과가 포함되어 있습니다.

```Python
# 숫자 범위 내에서 무작위 숫자 생성하기
import random
print(random.randint(1,10))

# 리스트에서 무작위로 아이템 선택하기
import random
list = ['Apple', 'Banana', 'Orange', 'Grape']
print(random.choice(list))
```

## 더 깊게 알아보기
무작위 숫자를 생성하는 방법에는 여러 가지가 있습니다. 일반적으로 사용되는 방법은 "Pseudorandom Number Generator"와 "True Random Number Generator" 두 가지입니다. 전자는 시드 값(seed value)을 기반으로 무작위 수를 생성하는 반면, 후자는 완전히 무작위로 수를 생성합니다. 이 중에는 보안 영역에서 사용할 수 있는 더 높은 수준의 보안성을 제공하는 "Quantum Random Number Generator"도 있습니다.

## 더 알아보기
- [Python - Random 모듈 문서](https://docs.python.org/ko/3/library/random.html)
- [Pseudorandom Number Generator에 대한 더 자세한 설명](https://www.geeksforgeeks.org/pseudorandom-number-generator-prng/)
- [True Random Number Generator에 대한 더 자세한 설명](https://www.geeksforgeeks.org/random-number-generator-true-random-number-generator-trng/)
- [Quantum Random Number Generator에 대한 더 자세한 설명](https://www.geeksforgeeks.org/quantum-random-number-generator-qrng/)