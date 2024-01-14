---
title:                "C: 문자열의 길이 찾기"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜
문자열의 길이를 찾는 것에 대해 왜 관심을 가져야 할까요? 그 이유는 바로 문자열이 프로그래밍에서 매우 중요하기 때문입니다. 문자열은 텍스트를 다루는 데 필수적이며, 많은 프로그램에서 사용되는 데이터 유형 중 하나입니다. 그렇기 때문에 문자열의 길이를 알고 있는 것은 매우 유용합니다.

## 하는 방법
C 프로그래밍에서 문자열의 길이를 찾는 것은 간단한 작업입니다. 우선 문자열의 시작 위치를 가리키는 포인터를 선언합니다. 그리고 포인터가 가리키는 값이 NULL이 아닐 때까지 계속 반복문을 실행합니다. 반복문 안에서 포인터를 하나씩 이동시키면서 문자열의 길이를 카운트하면 됩니다.

```C
// 입력 문자열
char str[] = "Hello World";
// 포인터 선언 및 초기화
char *ptr = str;
// 문자열의 길이를 나타낼 변수 선언 및 초기화
int length = 0;
// 포인터가 가리키는 값이 NULL이 아닐 때까지 반복
while (*ptr != '\0') {
    // 포인터 이동
    ptr++;
    // 길이 증가
    length++;
}
// 결과 출력
printf("String length: %d\n", length);
```

위 코드를 실행하면 "String length: 11"이라는 결과가 출력될 것입니다. 이를 보면 본인이 작성한 프로그램에서 문자열의 길이를 어떻게 구할 수 있는지 이해할 수 있을 것입니다.

## 깊이있게 알아보기
실제로 C 프로그래밍에서는 문자열의 길이를 구하는 방법이 더 다양하게 존재합니다. 위에서 사용한 while문 대신 for문을 사용하여 구현할 수도 있습니다. 또한, C 라이브러리에서 제공하는 strlen() 함수를 사용할 수도 있습니다. 이 외에도 문자열의 길이를 구하는 더 효율적인 방법들도 존재합니다. 따라서 자신에게 가장 적합한 방법을 찾아 사용해보는 것이 좋습니다.

## 참고 자료
- [C Programming Tutorial - Strings](https://www.programiz.com/c-programming/c-strings)
- [C Programming - String Length](https://www.tutorialspoint.com/c_standard_library/c_function_strlen.htm)
- [GeeksforGeeks - different ways to find length of a string in C/C++](https://www.geeksforgeeks.org/different-ways-find-length-string-c/)