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

## 무엇 & 왜?
난수 생성이란 무엇일까요? 간단히 말해, 난수 생성은 사람이 만들어내기 어려운 무작위 숫자를 프로그램을 통해 생성하는 것을 의미합니다. 여기에는 몇 가지 이유가 있는데, 예를 들면 게임에서 승리를 결정하기 위해 실제 무작위성을 재현하는 등의 목적이 있습니다. 

## 어떻게:
```Python
import random

# 1부터 10까지의 무작위 정수 생성
print(random.randint(1, 10))

# 0부터 1 사이의 무작위 실수 생성
print(random.random())

# 리스트에서 무작위 항목 선택
numbers = [1, 2, 3, 4, 5]
print(random.choice(numbers))
```

출력:

8

0.46236680138124066

3

## 깊게 파헤치기:
(1) 난수 생성의 역사적 배경: 난수 생성은 오래된 컴퓨팅 문제 중 하나입니다. 초기 컴퓨터에서는 속도와 비용 등의 제약으로 인해 무작위성을 재현하는 것이 매우 어려웠습니다. 
(2) 대체 가능한 방법: PRNG (Pseudo-Random Number Generator)은 난수라기보다는 무작위성을 재현한 것으로 보기에는 적합하지 않습니다.
(3) 구현 세부 정보: 파이썬의 random 모듈은 Mersenne-Twister 알고리즘을 사용하여 난수를 생성합니다. 이 알고리즘은 매우 빠르고, 통계적으로 안정적이며 사실적인 숫자를 생성하는 데에 적합합니다.

## 관련 자료:
- 파이썬 공식 문서: https://docs.python.org/3/library/random.html
- PRNG에 대한 자세한 설명: http://mathworld.wolfram.com/PseudorandomNumber.html
- 다른 난수 생성 알고리즘: https://www.geeksforgeeks.org/random-number-generator-generate-numbers-set-2/