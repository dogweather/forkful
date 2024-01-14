---
title:                "Python: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

우리는 프로그래밍을 할 때 종종 랜덤한 숫자를 생성해야 할 때가 있습니다. 이것은 게임에서 적절한 난수를 생성하는 것이거나, 암호화에서 키를 생성하는 경우 등 여러 가지 목적이 있습니다. 랜덤한 숫자를 생성하는 것은 우리가 다양한 프로그래밍 문제를 해결하는 데 도움이 될 수 있습니다.

## 어떻게

파이썬에서 랜덤한 숫자를 생성하는 방법은 간단합니다. 우리는 `random` 라이브러리를 사용하면 됩니다.

```python
import random

# 1부터 10까지의 랜덤한 정수 생성
random.randint(1, 10) 

# 0과 1 사이의 랜덤한 실수 생성
random.random() 
```

위의 예시는 간단한 랜덤 숫자 생성 방법입니다. `random` 라이브러리에는 더 다양한 함수가 있으니 필요에 따라 사용하시면 됩니다.

```python
# 리스트에서 랜덤한 요소 선택
random.choice(['a', 'b', 'c']) 

# 리스트에서 랜덤한 요소 3개 선택 (중복 허용)
random.choices(['a', 'b', 'c'], k=3) 

# 리스트에서 랜덤한 요소 3개 선택 (중복 불허용)
random.sample(['a', 'b', 'c'], k=3) 
```

## 더 들어가기

랜덤한 숫자를 생성하는 알고리즘은 여러 가지가 있지만, 대부분의 프로그래밍 언어에서는 Mersenne Twister 알고리즘을 사용합니다. 이 알고리즘은 매우 큰 사이클을 가지며, 충분히 큰 수를 랜덤하게 생성할 수 있습니다.

또한 실제로 랜덤한 숫자를 생성하려면 컴퓨터에서 발생하는 물리적인 변화를 이용해야 합니다. 이는 컴퓨터 내부의 랜덤한 환경 요소를 이용하는 것으로, 보안 관점에서 매우 중요합니다.

## 더 알아보기

우리는 이 블로그에서 파이썬에서 랜덤한 숫자를 생성하는 방법에 대해 알아보았습니다. 하지만 더 깊이 들어가서 랜덤 함수의 내부 구조나 알고리즘에 대해 자세히 알아보고 싶은 분들은 다음 링크들을 확인해 보실 수 있습니다.

[PyMotW: random](https://pymotw.com/3/random/index.html)  
[Python 3 Documentation: random](https://docs.python.org/3/library/random.html)  
[Real Python: How to Generate Random Numbers in Python](https://realpython.com/python-random/)  

## 더 알아보기