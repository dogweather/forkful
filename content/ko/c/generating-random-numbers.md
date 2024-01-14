---
title:                "C: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

랜덤한 숫자를 생성하는것이 왜 중요한지 아시나요? 랜덤한 숫자는 다양한 컴퓨터 프로그램에서 매우 유용하게 사용될 수 있습니다. 예를 들어서, 게임에서는 적의 위치나 아이템의 위치를 랜덤하게 생성하는데 사용될 수 있습니다. 즉, 프로그램을 더 실제같이 만들기 위해서 랜덤한 숫자를 사용할 수 있습니다. 그리고 랜덤한 숫자는 암호화 역시 사용될 수 있습니다. 즉, 랜덤한 숫자는 꽤 중요하게 사용될 수 있습니다.

## 방법

여러분이 랜덤한 숫자를 생성하기 위해서는 "stdio.h" 라는 라이브러리를 포함한 후에 ```C srand(time(NULL)); // 랜덤한 시드값 설정 // 랜덤한 1부터 10까지의 숫자 생성 printf("%d", rand() % 10 + 1); ``` 와 같은 코드를 작성할 수 있습니다. 이렇게 되면 1부터 10까지의 랜덤한 숫자가 출력이 됩니다. 여러분은 이 코드를 다양한 방법으로 사용할 수 있습니다. 예를들어 멀티플레이어 게임에서 플레이어들에게 랜덤한 숫자를 할당하는데 사용할 수 있습니다.

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // 랜덤한 시드값 설정
    srand(time(NULL));
    // 랜덤한 1부터 10까지의 숫자 생성
    printf("%d", rand() % 10 + 1);
    return 0;
}
```

## 깊이 파고들기

랜덤한 숫자를 생성하는 방법은 여러가지가 있습니다. 위에서 예시로 든 방법 외에도 여러분이 원하는 방식으로 랜덤한 숫자를 생성할 수 있습니다. 예를들어서 여러분이 랜덤한 수열을 만들고 싶을 때, 특정한 규칙을 가지고 랜덤한 수열을 생성할 수 있습니다. 그리고 랜덤한 숫자를 생성하는 알고리즘도 다양한 종류가 있습니다. 정해진 규칙에 따라서 랜덤한 숫자를 생성하는 방법도 여러분이고 한 알고리즘을 구현해보는 것은 매우 재미있을 것입니다.

## 참고 자료

- [How to Generate Random Numbers in C](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- [C Programming - Generating Random Numbers](https://www.programiz.com/c-programming/c-generate-random-numbers)
- [C Language: arc4random() function](https://www.techonthenet.com/c_language/standard_library_functions/stdlib_h/arc4random.php)