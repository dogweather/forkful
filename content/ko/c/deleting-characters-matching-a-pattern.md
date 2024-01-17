---
title:                "패턴과 일치하는 문자 삭제하기"
html_title:           "C: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?: 
이번에 배우게 될 것은 특정 패턴과 일치하는 문자를 제거하는 방법입니다. 프로그래머들이 이를 하는 이유는 코드의 가독성을 높이고, 불필요한 문자를 제거하여 코드를 더 간결하게 만들기 위해서입니다.

## 어떻게?:
```C
#include <stdio.h>
#include <string.h>

int main() {
  char str[] = "Hello! How are you?";
  
  //특정 패턴('o')과 일치하는 문자를 제거
  char *p = strchr(str, 'o');
  while (p) {
    memmove(p, p + 1, strlen(p));
    p = strchr(str, 'o');
  }

  printf("%s\n", str);
  //출력 결과: Hell! Hw are yu?
  
  return 0;
}
```

## 깊이 파고들기:
- 이 기법은 문자열 처리에서 딱히 새로운 것은 아닙니다. 예전부터 사용되어온 기법이지만 여전히 유용하게 쓰이고 있습니다.
- 다른 대안으로는 특정 문자를 다른 문자로 대체하는 방법도 있습니다. 이 방법은 전체 문자열을 검사해야 하는 번거로움이 있지만, 대체하려는 문자가 적다면 더 유용한 방법일 수도 있습니다.
- 이 기법을 구현할 때에는 포인터와 메모리의 이동 연산을 잘 이해해야 합니다. 그렇지 않으면 올바른 결과를 얻을 수 없을 수 있습니다.

## 더 알아보기:
- [strchr() 함수](https://www.tutorialspoint.com/c_standard_library/c_function_strchr.htm)의 공식 문서를 참고해보세요.
- [memmove() 함수](https://www.tutorialspoint.com/c_standard_library/c_function_memmove.htm)의 공식 문서를 참고해보세요.