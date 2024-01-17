---
title:                "문자열의 길이 찾기"
html_title:           "C++: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 스트링 길이 찾기

이번에는 C++ 프로그래밍에서 중요한 기술 중 하나인 "스트링 길이 찾기"에 대해 알아보도록 하겠습니다. C++에서 스트링 길이를 찾는 것은 프로그래머들이 자주 하는 작업 중 하나입니다.

## 무엇이며 왜?

스트링 길이 찾기란 단어 그대로 주어진 스트링의 길이를 찾는 것을 말합니다. 대부분의 프로그래밍 언어에서 스트링의 길이를 알아내는 함수를 제공하며, 이는 뒤에 이어질 예제에서도 사용될 것입니다. 프로그래머들은 스트링의 길이를 알고 있어야만 이를 적절하게 다룰 수 있고, 버그를 방지할 수 있습니다.

## 방법:

이제 C++에서 스트링 길이를 찾는 방법을 알아보겠습니다. 아래 예제를 보고 어떤 식으로 동작하는지 살펴보세요.

```C++
#include <iostream> 
#include <string> 
using namespace std;
  
int main() 
{ 
    string str = "안녕하세요"; 
    int len = str.length(); 
    cout << "스트링의 길이는 " << len << "입니다."; 
    return 0; 
} 
```

위 예제에서는 `<string>` 헤더 파일을 사용하여 스트링을 다루는 `length()` 함수를 사용합니다. 이 함수는 해당 스트링의 길이를 반환해줍니다. 위 예제에서는 "안녕하세요"라는 스트링의 길이를 알아낸 후 이를 출력하는 간단한 예제입니다.

## 깊게 들어가기:

이제 스트링 길이 찾기를 더 깊게 들어가보겠습니다.

### 역사적인 배경:

스트링 길이를 찾는 함수는 프로그래밍 언어가 만들어지기 전부터 존재했습니다. 그 동안 프로그래머들은 이를 직접 구현하거나 라이브러리를 이용하여 사용해왔습니다.

### 대안:

C++의 `<string>` 헤더 파일에는 `length()` 함수 외에도 `size()` 함수가 존재합니다. 이 두 함수는 기본적으로 동일한 기능을 수행하며, 어떤 것을 사용해도 큰 차이는 없습니다. 대부분의 프로그래머들은 그냥 편한 함수를 사용하기 때문에 어떤 함수를 사용해도 상관 없습니다.

### 구현 세부사항:

스트링 길이를 찾는 함수는 내부적으로 반복문을 이용하여 스트링의 길이를 하나씩 세어나갑니다. 또한 변수 형식이나 해당 스트링의 인코딩 방식 등에 따라 동작 방식이 다를 수 있습니다.

## 참고 자료:

- [C++ String Length Documentation](https://www.cplusplus.com/reference/string/string/length/)
- [Difference between size and length in string C++](https://stackoverflow.com/questions/37496792/difference-between-size-and-length-in-string-c)