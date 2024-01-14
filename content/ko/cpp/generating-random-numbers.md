---
title:    "C++: 랜덤 숫자 생성"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜?: 난수 생성에 참여하는 이유

난수 생성을 통해 많은 유용한 시나리오를 작성할 수 있습니다. 예를 들어, 당신이 게임 혹은 시뮬레이션 프로그래밍을 한다면 무작위로 생성된 그래픽이나 캐릭터를 만들 수 있습니다. 이는 게임에 더 많은 다양성과 진화를 더해줄 것입니다. 또한 난수 생성을 통해 랜덤한 암호를 생성하거나 랜덤한 이벤트를 발생시킬 수도 있습니다.

## 어떻게?: C++로 난수 생성하기

```C++
#include <iostream>
#include <cstdlib>
#include <ctime>
using namespace std;

int main() {
    // 시드 설정
    srand(time(0));

    // 1부터 100까지의 숫자 중에서 무작위로 선택
    int random_num = rand() % 100 + 1;

    // 결과 출력
    cout << "랜덤 숫자: " << random_num << endl;

    return 0;
}
```

출력 예시:
```
랜덤 숫자: 74
```

## 딥 다이브: 난수 생성에 대해 더 알아보기

난수 생성은 주어진 범위 내에서 무작위로 숫자를 선택하는 것으로 위에서 보여준 코드와 같이 `rand()` 함수를 사용하면 됩니다. 그러나 이 값을 더 임의적으로 만들기 위해선 시드 값을 설정해야 합니다. 시드 값은 난수 생성의 시작점이라고 생각하면 됩니다. 만약 시드 값을 설정하지 않으면 컴퓨터는 프로그램이 시작할 때마다 같은 순서로 숫자를 선택하게 됩니다.

따라서 보다 더 임의적인 난수를 생성하기 위해선 시간, 사용자 입력 혹은 프로세스 ID 등의 값을 시드 값으로 설정하는 것을 권장합니다. 또한 `srand()` 함수를 통해 시드 값을 설정하는 방법도 있습니다.

## 더 보기

- [C++ 랜덤 숫자 생성하기: rand() 함수 사용하기](https://dojang.io/mod/page/view.php?id=619)
- [난수 생성 원리와 시드 값 설정하기](https://modoocode.com/221)
- [랜덤 컨트롤러로서의 난수 생성](https://www.cs.hmc.edu/~geoff/classes/hmc.cs070.200101/homework10/randguys.html)