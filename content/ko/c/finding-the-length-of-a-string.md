---
title:    "C: 문자열의 길이 찾기"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 길이를 찾는 것은 C 프로그래밍에서 중요한 작업입니다. 문자열 길이를 알아내는 것은 원하는 정보를 찾는데 매우 유용합니다. 예를 들어, 문자열 안에 몇 개의 글자가 있는지 알고 싶을 때, 문자열의 길이를 찾아야 합니다.

## 하는 법

```
#include <stdio.h>
#include <string.h>

int main()
{
    char str[] = "안녕하세요!";  // 찾으려는 문자열
    int length = strlen(str);   // 길이를 저장할 변수

    printf("문자열의 길이는 %d입니다.", length);
    return 0;
}

// 출력 결과: "문자열의 길이는 7입니다."
```

위의 예제는 C 언어에서 문자열의 길이를 찾는 간단한 방법입니다. ```string.h``` 헤더 파일의 ```strlen()``` 함수를 사용하여 문자열의 길이를 구하고, 결과를 출력하는 코드입니다. ```strlen()``` 함수는 문자열의 길이를 알려주는 정수 값을 반환합니다.

## 깊이 파고들기

C 프로그램에서 문자열의 길이를 찾는 과정은 매우 간단하게 보일 수 있지만, 실제로는 어떻게 동작하는지 깊이 이해하는 것이 중요합니다. 문자열의 길이를 찾는 방법은 다음과 같은 과정으로 이루어집니다.

1. ```strlen()``` 함수는 문자열의 첫 번째 글자부터 쭉 탐색합니다.
2. 문자열의 마지막 글자를 만나면, 모든 글자를 탐색한 것으로 간주합니다.
3. 탐색한 모든 글자의 개수를 세어서 결과 값을 반환합니다.

예제 코드에서도 알 수 있듯이, 문자열의 길이를 찾는 과정은 매우 간단하고 효율적입니다. 하지만 알고리즘의 원리를 이해하는 것은 더 복잡한 작업을 수행할 때 더 유용합니다.

## 참고

- [C 언어 문자열 처리](https://dojang.io/mod/page/view.php?id=509)
- [strlen() 함수](https://ko.wikipedia.org/wiki/Strlen())
- [문자열 처리와 함수](http://norux.me/64)