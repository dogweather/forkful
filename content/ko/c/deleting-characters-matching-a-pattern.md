---
title:                "패턴에 해당하는 문자 삭제"
html_title:           "C: 패턴에 해당하는 문자 삭제"
simple_title:         "패턴에 해당하는 문자 삭제"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

컴퓨터 프로그래밍에서 가장 중요한 부분 중 하나는 적절한 데이터 처리입니다. 때때로 우리는 특정 패턴에 일치하는 문자를 삭제하는 작업을 해야 할 수도 있습니다. 이럴 때 우리는 수많은 선택지 중에 어떤 것을 선택해야 할지 모를 수 있습니다. 그래서 이번 기사에서는 우리가 왜 문자를 삭제하는 작업을 하는지 알아보고, 어떻게 그 작업을 수행하는지 살펴보겠습니다.

## 어떻게

문자를 삭제할 때는 먼저 삭제할 패턴을 정의해야 합니다. 이 패턴은 정규 표현식을 사용하여 표현할 수 있습니다. 그리고 우리는 이 패턴에 일치하는 문자를 찾아서 삭제하게 됩니다.

아래는 C 프로그래밍 언어를 사용하여 문자를 삭제하는 예제 코드입니다. 먼저 정규 표현식을 사용하여 "a" 또는 "b"로 시작하는 문자를 찾고, 그 문자가 있을 경우 삭제하는 코드입니다.

```C
#include <stdio.h>
#include <string.h>

int main() {
  char string[] = "abcd1234efg57";

  printf("Before deletion: %s\n", string);

  // 정규 표현식 "^[ab]"를 사용하여 문자를 삭제합니다.
  // "a" 또는 "b"로 시작하는 문자를 찾아서 삭제하게 됩니다.
  for (int i = 0; i < strlen(string); i++) {
    if (string[i] == 'a' || string[i] == 'b') {
      memmove(&string[i], &string[i + 1], strlen(string) - i);
      i--;
    }
  }

  printf("After deletion: %s\n", string);

  return 0;
}
```

위 코드를 실행하면 아래와 같은 출력이 나오게 됩니다.

```
Before deletion: abcd1234efg57
After deletion: cd1234efg57
```

## 깊게 파고들기

이번에는 문자를 삭제하는 과정을 좀 더 자세하게 살펴보겠습니다. 먼저 문자의 삭제는 메모리에 저장된 데이터를 다루는 작업입니다. 따라서 문자를 삭제하는 과정에서는 메모리에 저장된 데이터를 조작하는 방법을 알아야 합니다.

우리는 문자를 삭제할 때 주로 for 루프를 사용하여 문자열을 반복하면서 원하는 패턴에 일치하는 문자를 찾아서 삭제합니다. 이 때 주의해야할 것은 삭제한 문자 이후의 문자들을 메모리에서 옮겨줘야 한다는 것입니다. 따라서 memmove 함수를 사용하여 문자를 옮겨주는 작업을 해야 합니다.

또한, 정규 표현식을 사용하여 문자를 삭제하면 다양한 패턴을 한 번에 처리할 수 있기 때문에 유용합니다. 이를 통해 간단한 규칙으로도 다양한 문자를 삭제할 수 있습니다.

## 또 다른 정보

만약 여러분이 C 프로그래밍 언어를 처음 배우시는 분이라면, 문자열과 문자열 처리에 대한 기본적인 지식을 먼저 익힌 다음에 이 기술을 적용하는 것이 좋습니다. 또한 정규 표현식에 대한 이해도 중요하므로 이에 대해서도 공부하는 것이 좋습니다.

## 더 알아보기

- 정규 표현식 문법: https://regexone.com/
- C 프로그래밍 언어 공식 문서: https://devdocs.io