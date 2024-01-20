---
title:                "임의의 숫자 생성하기"
html_title:           "Elixir: 임의의 숫자 생성하기"
simple_title:         "임의의 숫자 생성하기"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜 & 왜?
랜덤 숫자 생성은 예측 가능한 코드흐름을 방해하는 데 사용될 수 있습니다. 프로그래머들은 테스트 데이터를 생성하거나, 간단한 게임을 제작하거나 사용자의 입력을 시뮬레이트하는 등 다양한 이유로 랜덤 숫자를 생성합니다.

## 사용법:
Python에서 랜덤 숫자를 생성하는 대표적인 방법은 `random` 라이브러리를 사용하는 것입니다. 아래의 코드는 0과 10 사이에서 랜덤 정수를 생성하는 방법을 보여줍니다.

```Python
import random

random_number = random.randint(0, 10)
print(random_number)
```

출력 예시는 아래와 같습니다.

```
5
```

## 상세 분석:
랜덤 숫자는 어떤 배열이나 패턴이 없는 이른바 '차수 없는' 숫자입니다. 랜덤 숫자 생성은 컴퓨팅에 고대부터 있었으며, 예를 들어, 사람들이 도박이나 복권 번호를 선택하는 데 사용한 것입니다.

Python에서는 `random`라는 기본 라이브러리를 제공합니다. 이 라이브러리는 메르센 트위스터라고 불리는 알고리즘을 사용하여 숫자를 생성합니다. 이 방법의 대안으로 numpy 라이브러리의 `numpy.random` 모듈을 사용할 수 있습니다. 이것은 더 복잡한 통계적 분포를 가진 랜덤 숫자를 생성하는 데 유용합니다.

## 참고 자료:
- Python 공식 문서: [random — Generate pseudo-random numbers](https://docs.python.org/3/library/random.html)
- Numpy 공식 문서: [numpy.random — Generate random numbers](https://numpy.org/doc/stable/reference/random/index.html)