---
title:                "랜덤 숫자 생성하기"
html_title:           "C++: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 랜덤 숫자 생성 및 그 이유
랜덤 숫자를 생성하는 것은 프로그래머들이 많이 하는 일입니다. 보통 이 작업을 통해 우리는 컴퓨터가 제공하는 예측 불가능한 숫자를 사용할 수 있게 됩니다. 이를 사용하는 이유는 다양한 응용 프로그램에서 다양한 입력 값을 생성하기 위해서 입니다. 예를 들어 게임에서 적들의 위치를 랜덤하게 생성하거나, 암호화를 위해 랜덤한 키를 생성하는 등 다양한 목적으로 사용됩니다.

# 방법:
랜덤 숫자를 생성하는 방법에는 여러 가지가 있지만, 여기서는 가장 일반적인 ```rand()``` 함수를 사용하는 방법을 살펴보겠습니다. 이 함수는 헤더 파일 ```cstdlib```에 정의되어 있으며, 0에서 RAND_MAX 사이의 임의의 정수를 반환합니다. 아래 예제를 통해 사용 방법을 살펴보겠습니다.

```c++
#include <iostream>
#include <cstdlib>

int main()
{
    // 0에서 RAND_MAX 사이의 랜덤한 정수 생성
    int randomNumber = rand();

    // 생성된 숫자 출력
    std::cout << "랜덤한 숫자: " << randomNumber << std::endl;

    return 0;
}
```

위 코드를 실행하면 다음과 같은 출력 결과를 얻게 됩니다.

```
랜덤한 숫자: 28394
```

# 더 알아보기
랜덤 숫자를 생성하는 방법에는 여러가지가 있지만, 옛날부터 많이 사용되고 있는 방법 중에 하나는 난수 생성기를 사용하는 것입니다. 난수 생성기는 일반적으로 시드(seed)라는 초기 값과 알고리즘을 사용하여 순환하는 값들을 이용해 무작위 수열을 생성합니다. 또 다른 방법으로는 물리적인 요소를 이용하는 방식도 있습니다. 예를 들어 마우스의 움직임이나 마우스 버튼을 누르는 시간 등을 이용해 랜덤한 값을 생성할 수 있습니다.

# 관련 링크
- [C++ 표준 라이브러리 랜덤 함수](https://en.cppreference.com/w/cpp/numeric/random)
- [난수 생성기의 역사](https://www.geeksforgeeks.org/history-of-random-number-generators/)
- [물리적인 요소를 이용한 난수 생성](https://en.wikipedia.