---
title:    "Python: 랜덤 숫자 생성"
keywords: ["Python"]
---

{{< edit_this_page >}}

# 왜 무작위 숫자를 생성할까요?

무작위 숫자를 생성하는 것은 다양한 목적과 상황에서 인간에게 도움이 될 수 있습니다. 예를 들어, 데이터 분석, 통계학, 머신러닝, 게임 등에서 무작위 숫자는 매우 중요한 요소입니다. 무작위 숫자를 생성하는 것은 다양한 분야에서 활용도가 높기 때문에 프로그래밍에서도 중요한 개념입니다.

## 어떻게 하나요?

파이썬에서 무작위 숫자를 생성하는 방법은 다양합니다. 여기에서는 `random` 라이브러리를 사용하는 방법을 소개합니다. 이 라이브러리에는 다양한 함수가 존재하며, 각각 다른 종류의 무작위 숫자를 생성할 수 있습니다.

```Python
import random

# 0 이상 1 미만의 무작위 실수 생성
print(random.random())

# 1 이상 10 미만의 무작위 정수 생성
print(random.randint(1, 10))

# 주어진 리스트에서 무작위로 요소 선택
fruits = ['apple', 'banana', 'orange', 'grape']
print(random.choice(fruits))
```

위 코드를 실행하면 다음과 같은 결과가 나옵니다.

```Python
0.7933903119969373
5
orange
```

## 딥 다이브

무작위 숫자를 생성하는 알고리즘에는 여러 가지가 있지만, 대표적으로 유사난수 생성기라는 것이 있습니다. 이는 사람이 무작위로 선택하는 것처럼 보이지만 실제로는 일정한 규칙에 따라 생성된 숫자들을 사용하는 것입니다. 또한, 컴퓨터에서 완전한 무작위 숫자를 생성하는 것은 불가능합니다. 그러나 충분히 무작위에 가까운 숫자를 생성할 수 있기 때문에 다양한 분야에서 활용됩니다.

# 또 보기

- [Python 랜덤 모듈 공식 문서](https://docs.python.org/3/library/random.html)
- [파이썬 랜덤화에 대한 이야기](https://realpython.com/python-random/)
- [컴퓨터에서 완전한 난수를 만들 수 있을까요?](https://physics.stackexchange.com/questions/326534/can-a-computer-generate-true-random-numbers)