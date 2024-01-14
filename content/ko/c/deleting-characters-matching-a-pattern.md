---
title:    "C: 패턴과 일치하는 문자 삭제하기"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 왜

삭제 조건과 일치하는 문자를 삭제하는 것의 중요성을 간단하게 설명합니다.

## 어떻게 할까요

아래 코드는 C 언어로 작성된 예제 코드이며, 원하는 출력 결과도 함께 제시됩니다.

```C
#include <stdio.h>
#include <string.h>

int main() {
  char string[] = "Hello, world!"; // 문자열 선언
  int i, j; // 반복문에서 사용할 변수

  for (i = 0; i < strlen(string); i++) { // 문자열의 길이만큼 반복
    if (string[i] == 'l') { // 현재 인덱스의 문자가 'l'인지 확인
      for (j = i; j < strlen(string); j++) { // 인덱스부터 문자열의 길이만큼 반복
        string[j] = string[j+1]; // 문자를 삭제하고 뒤에 있는 문자들을 한 칸씩 앞으로 이동
      }
    }
  }

  printf("%s", string); // 변경된 문자열을 출력

  return 0;
}
```

위의 코드는 문자열에서 모든 'l'을 삭제하는 예제입니다. 출력 결과는 `Heo, word!`가 됩니다.

## 깊이있게 살펴보기

해당 기능을 직접 구현하는 것보다는 이미 구현된 표준 라이브러리 함수를 사용하는 것이 더 간편합니다. C 언어에서는 `string.h` 헤더 파일에 포함된 `strchr()` 함수를 사용하면 됩니다. `strchr()` 함수는 문자열에서 첫 번째로 발견되는 특정 문자의 위치를 반환해주는 함수입니다.

```C
#include <stdio.h>
#include <string.h>

int main() {
  char string[] = "Hello, world!"; // 문자열 선언
  char *ptr; // 문자열의 주소 값을 반환받을 포인터 변수

  ptr = strchr(string, 'l'); // 문자열에서 'l'이 처음 발견된 위치를 포인터 변수에 저장

  while (ptr != NULL) { // 발견된 위치가 NULL이 아닐 때까지 반복
    strcpy(ptr, ptr + 1); // 발견된 문자를 삭제하고 뒤에 있는 문자들을 한 칸씩 앞으로 이동
    ptr = strchr(string, 'l'); // 다시 'l'이 발견된 위치를 찾아서 포인터에 저장
  }

  printf("%s", string); // 변경된 문자열을 출력

  return 0;
}
```

위의 코드는 `strchr()` 함수를 사용하여 문자열에서 모든 'l'을 삭제하는 예제입니다. 출력 결과는 `Heo, word!`가 됩니다.

# 더 자세히 알아보기

해당 기능을 다양한 방법으로 구현할 수 있지만, 가장 효율적인 방법은 표준 라이브러리 함수를 사용하는 것입니다. `string.h` 헤더 파일에는 문자열을 검색하고, 복사하고, 비교하고, 변경하는 데에 유용한 다양한 함수들이 포함되어 있습니다. 이러한 함수들을 잘 활용하면 프로그래밍을 더 쉽게 할 수 있습니다.

# 또 다른 예제

- [C 언어 문자열에서 모음 제거하기](https://www.programiz.com/c-programming/examples/remove-vowels-string)
- [C 언어 문자열에서 중복 문자 제거하기](https://www.tutorialspoint.com/c-program-to-remove-duplicates-from-a-string)
- [C++ 언어 문자열에서 특정 단어 치환하기](https://www.geeksforgeeks.org/cpp-program-find-replace-given-word-text-file/)
- [Java 언어 문자열에서 숫자 제거하기](https://www.baeldung.com/java-string-remove-number