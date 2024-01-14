---
title:                "C: 문자열 연결"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열을 연결하는 것에 대해 이야기하기 전에 우리는 우리가 왜 그렇게해야하는지에 대해 알아야합니다. 문자열 연결은 우리가 더 큰 문자열을 만들기 위해 작은 문자열을 결합할 때 유용합니다. 예를 들어, 우리가 이름과 성을 각각 따로 저장하고 싶을 때, 두 개의 문자열을 연결하여 전체 이름을 얻을 수 있습니다.

## 하는 방법

C 프로그램에서 문자열을 연결하는 것은 매우 간단합니다. 다음은 두 개의 문자열을 연결하는 예시 코드입니다.

```C
#include<stdio.h>
#include<string.h>

int main()
{
     char first_name[20] = "지수";
     char last_name[20] = "김";

     strcat(first_name, last_name);
     printf("전체 이름: %s", first_name);

     return 0;
}
```

위의 코드를 실행하면 "전체 이름: 지수김"이라는 출력을 볼 수 있습니다. 우선, 우리는 두 개의 문자열을 저장하기 위해 `first_name`과 `last_name` 변수를 선언합니다. 그런 다음 `strcat` 함수를 사용하여 두 번째 문자열을 첫 번째 문자열에 연결합니다. 마지막으로 `printf` 함수를 사용하여 전체 이름을 출력합니다.

## 깊이 들어가기

C 프로그램에서 문자열을 연결하는 주요 함수는 `strcat` 함수입니다. 이 함수는 두 개의 매개변수를 받습니다. 첫 번째 매개변수는 대상 문자열이고, 두 번째 매개변수는 추가 될 문자열입니다. `strcat` 함수는 첫 번째 문자열의 끝에 두 번째 문자열을 붙여줍니다. 이때 첫 번째 매개변수로 사용된 문자열의 크기는 충분히 크도록 선언되어야합니다. 그렇지 않으면 두 번째 문자열이 긴 경우 오류가 발생할 수 있습니다.

## 또 다른 참고 자료

- [C 문자열 연결하기](https://www.programiz.com/c-programming/library-function/string.h/strcat) 
- [C 문자열 함수에 관한 자세한 내용](https://www.cs.cf.ac.uk/Dave/C/node16.html) 
- [C 프로그래밍 튜토리얼](https://www.tutorialspoint.com/cprogramming/index.htm)

## 참고하기

- 이 포스트는 문자열을 연결하는 방법에 대한 간략한 소개였습니다. 더 많은 문자열 관련 함수를 배우고 싶다면 위의 참고 자료를 확인해보세요.
- C 프로그래밍에서는 문자열 뿐만 아니라 다른 자료형도 연결 할 수 있습니다. 이를 위해 `strcat` 함수 대신 `sprintf` 함수를 사용할 수 있습니다.
- 프로그래밍에서 문자열을 연결하는 것은 매우 유용합니다. 하지만 문자열을 연결할 때는 항상 문자열의 크기를 고려해야합니다. 충분한 공간이 없는 경우 오류가 발생할 수 있습니다.