---
title:    "Python: 랜덤 숫자 생성"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜 무작위 숫자 생성에 참여해야 하는가?

우리 주위에는 많은 예측 가능한 패턴이 있습니다. 하지만 예측 불가능한 숫자를 필요로 할 때가 있습니다. 예를 들어 무작위 번호 생성은 보안 및 보안 검증에 사용될 수 있습니다. 또한 테스트 및 실험을 할 때 무작위 숫자를 사용하여 특정 상황에서 무엇이 발생할 수 있는지 알 수 있습니다.

## 무작위 숫자 생성하는 방법

```Python
# numpy 라이브러리를 불러옵니다
import numpy as np

# 0과 1 사이에서 무작위 숫자 3개를 생성합니다
random_nums = np.random.rand(3)
print(random_nums)
```

이 코드를 실행하면 다음과 같은 결과가 나옵니다:

[0.37672483 0.55425849 0.16253995]

또는 다음과 같이 범위를 지정하여 숫자를 생성할 수도 있습니다:

```Python
# 1부터 10 사이에서 무작위 숫자 5개를 생성합니다
random_nums = np.random.randint(low=1, high=10, size=5)
print(random_nums)
```

이 코드를 실행하면 다음과 같은 결과가 나옵니다:

[4 8 2 7 9]

## 무작위 숫자 생성에 대해 더 알아보기

무작위 숫자 생성은 컴퓨터 과학에서 중요한 역할을 합니다. 이를 통해 다양한 알고리즘 및 시뮬레이션을 구현할 수 있습니다. 그리고 무작위 숫자 생성은 여러 분야에서 다양한 연구 및 응용프로그램에 사용됩니다. 하지만 무작위성의 개념 자체는 상대적이기 때문에 무작위 숫자를 생성하는 알고리즘은 항상 완벽하게 무작위하지는 않습니다. 따라서 항상 신뢰할 수 있는 라이브러리 및 메서드를 사용하는 것이 중요합니다.

## 참고 링크

- [numpy 라이브러리 문서](https://numpy.org/doc/)
- [Python 무작위 숫자 생성 함수](https://docs.python.org/3/library/random.html)
- [무작위 숫자 생성의 이해](https://www.geeksforgeeks.org/understanding-python-random-number-generator/)