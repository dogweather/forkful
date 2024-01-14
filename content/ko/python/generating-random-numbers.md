---
title:                "Python: 랜덤 숫자 생성"
programming_language: "Python"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

##왜

난수(random numbers)를 생성하는 것에 대해 의문을 가지고 있는 분들도 있을 것입니다. 하지만 소프트웨어 개발에서 가끔은 우리가 상황에 따라 난수를 필요로 할 때가 있습니다. 예를 들어 보세요, 시뮬레이션을 만들고 싶은 경우나, 암호화를 구현해야 하는 경우, 또는 간단히 재미있는 게임을 만들어야 할 때 등등. 이러한 상황에서 난수를 사용하면 우리의 코딩 생활을 더 흥미롭게 만들 수 있습니다.

##방법

일반적으로 난수를 생성하는 방법은 무작위 값이 필요한 데이터를 포함하는 함수를 사용하는 것입니다. 예를 들어, Python에서는 `random` 모듈을 사용하여 다양한 형태의 난수를 생성할 수 있습니다. 아래의 예시 코드를 보며 어떻게 쉽게 난수를 생성하는지 확인해보세요.

```Python
import random

# 0부터 1사이의 실수형 난수 생성
random.random()

# 특정 범위의 정수형 난수 생성
random.randint(1, 100)

# 리스트에서 임의의 요소 선택
list = ["apple", "orange", "banana"]
random.choice(list)
```

위 코드를 실행하면 각각 0과 1 사이의 난수, 1에서 100 사이의 정수 난수, 그리고 리스트의 임의의 요소가 출력될 것입니다.

##더 깊이 파고들어보기

실제로 난수를 생성하는 방법은 아주 복잡합니다. 현재 시간, 물리적인 소리, 또는 컴퓨터 시드(seed)와 같은 다양한 요소들을 활용하여 무작위 값을 생성하게 됩니다. 그리고 이러한 방식으로 생성한 난수가 고도로 예측 불가능하고 안전하기 때문에 암호학적인 목적으로도 자주 사용됩니다.

실제로 우리가 사용하는 컴퓨터에서는 정말로 무작위적인 난수를 생성할 수 없기 때문에 알고리즘에 따라 생성된 겉으로는 난수처럼 보이지만 사실상 예측 가능한 시퀀스(sequence)가 생성될 수도 있습니다. 따라서 보안이 중요한 경우에는 암호학적으로 안전한 난수 생성기를 사용하는 것이 좋습니다.

##관련 자료

- [Python 공식 문서 - random 모듈](https://docs.python.org/ko/3/library/random.html)
- [난수 생성기를 사용하면서 겪을 수 있는 일들](https://m.blog.naver.com/since201110/221725478860)
- [암호학적으로 안전한 난수 생성기 예시 - CryptGenRandom](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=netcore-3.1)