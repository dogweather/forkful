---
title:                "문자열 연결하기"
html_title:           "C: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/concatenating-strings.md"
---

{{< edit_this_page >}}

# 왜

문자열을 연결하는 행위를 왜 하는지 알고 싶으시죠? 그렇다면 이 글이 여러분에게 도움이 될 거에요!

# 사용 방법

문자열을 연결하는 방법은 매우 간단합니다. 다음과 같이 두 가지 문자열을 `+` 연산자로 이어주면 됩니다.

```C
char str1[] = "Hello ";
char str2[] = "world!";
printf("%s%s", str1, str2);
```

위 코드의 결과는 `Hello world!`가 됩니다.

아래는 더 복잡한 예제입니다. `strcat` 함수를 사용하여 두 문자열을 연결하고, `strcpy` 함수를 사용하여 새로운 변수에 복사하는 방법을 보여드리겠습니다.

```C
char str1[] = "I ";
char str2[] = "love ";
char str3[] = "programming.";
strcat(str1, str2);
char newStr[20];
strcpy(newStr, str1);
strcat(newStr, str3);
printf("%s", newStr);
```

이 코드의 결과는 `I love programming.`이 됩니다.

# 깊게 들어가보기

위에서는 간단한 예제만을 보여드렸지만, 실제로 문자열을 연결하는 과정에서는 C 언어의 메모리 관리 기능을 이해해야 합니다. 문자열을 연결할 때 메모리 공간을 동적으로 할당해야 하며, 그리고 할당했던 메모리 공간을 중복 사용하지 않도록 주의해야 합니다. 이러한 과정을 통해 메모리 누수를 방지하고 효율적인 프로그래밍을 할 수 있습니다.

# 관련 자료

- [C 언어 문자열](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [메모리 동적 할당](https://modoocode.com/241)