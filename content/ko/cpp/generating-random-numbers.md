---
title:                "C++: 랜덤 숫자 생성하기"
programming_language: "C++"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

난수 생성에 관심을 갖는 이유는 무엇일까요? 난수는 가위바위보를 할 때 나오는 선택의 결과와 같이 예측할 수 없는 부분이 있는 모든 분야에서 유용하게 사용될 수 있습니다.

## 어떻게 하나요?

난수를 생성하는 가장 간단한 방법은 C++의 ```rand()``` 함수를 사용하는 것입니다. 이 함수는 0부터 RAND_MAX 사이의 난수를 생성해줍니다. 여기서 RAND_MAX는 C++ 표준 라이브러리에서 정의된 최대 난수 값입니다. 다음은 ```rand()``` 함수를 이용해 0부터 10까지의 난수를 생성하는 예시 코드입니다.

``` C++
#include <iostream>
#include <cstdlib>

int main() {
  int random_num = rand() % 11; // 0부터 10까지의 난수 생성
  std::cout << "난수가 생성되었습니다: " << random_num << std::endl;
  return 0;
}
```

위 코드를 실행하면 아래와 같은 결과를 얻을 수 있습니다.

```
난수가 생성되었습니다: 8
```

하지만 위의 예시 코드는 항상 똑같은 난수를 생성하기 때문에 정말로 난수인가 싶을 수 있습니다. 따라서 보다 더 복잡한 난수를 원한다면 C++ 라이브러리에서 제공하는 다른 함수들을 이용해야 합니다. 예를 들어, ```srand()``` 함수를 사용해 난수의 시드 값을 설정하거나, ```uniform_int_distribution``` 클래스를 이용해 특정 범위 내에서 균등한 난수를 생성할 수 있습니다. 더 자세한 내용은 아래 "깊이 파고들기" 섹션을 참고해주세요.

## 깊이 파고들기

난수를 생성하는 방법은 여러 가지가 있습니다. 그 중에서도 C++ 라이브러리에서 제공하는 랜덤 넘버 발생 메커니즘 중 가장 안전한 방법은 "어디에도 연관성이 없는" 랜덤 비트를 사용하는 것입니다. 이를 쉽게 이해하기 위해 랜덤 숫자 발생기는 "플레이아웃 머신"으로 생각하면 됩니다. 기본적으로 일련의 알고리즘을 통해 조금 느릴 수는 있지만 충분히 큰 주기를 갖도록 정상적으로 초기화된 상태에서 튜브 안의 중어터 비트를 제공하는 내부 온자를 사용해서 마치 무작위로 난수를 섞고 발행하는 것과 같습니다. 따라서 시드 값과 난수 발생기가 초기화된 상태와 동일한 비트를 사용하면 언제나 같은 결과를 얻을 수 있습니다. 그렇기 때문에 사용자는 발생기를 초기화하는 데에 충분한 비트를 제공하는 것만으로도 코드를 정상적으로 작동할 수 있도록 하는 조건을 지키면 됩니다.

## 참고

- [C++ Reference - rand()](https://en.cppreference.com/w/cpp/numeric/random/rand)
- [C++ Reference - srand()](https://en.cppreference.com/w/cpp/numeric/random/srand)
- [C++ Reference - uniform_int_distribution](https://en.cppreference.com/w/cpp/numeric/random/uniform_int_distribution)
- [Understanding Random Number Generators in C++](https://www.codeproject.com/Articles/1181775/Understanding-Random-Number-Generators-in-Cplusplus)