---
title:                "문자열 연결하기"
html_title:           "C++: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

문자열 연결(concatenating strings)이란 무엇일까요? 간단히 말해서, 문자열 연결은 여러 개의 문자열을 하나의 문자열로 합치는 것을 말합니다. 프로그래머들이 문자열 연결을 하는 이유는, 많은 데이터를 다룰 때 유용하기 때문입니다. 예를 들어, 여러 개의 성과 이름을 가진 사람들의 목록을 만들 때, 각각의 성과 이름을 합쳐서 하나의 문자열로 만들면 더 편리하게 관리할 수 있습니다.

# 하는 법:

```C++
// 먼저, 두 개의 문자열을 변수에 저장합니다.
string first = "Hello";
string second = "World";

// 두 문자열을 연결하여 새로운 문자열을 만들고, 변수에 저장합니다.
string concatenated = first + second;

// 새로운 문자열의 출력을 확인합니다.
cout << concatenated << endl;
```
출력: HelloWorld

# 깊게 들어가보기:

1. 역사적 배경: C 언어에서 문자열을 다루는 방식은 비효율적이었습니다. 따라서 C++에서는 문자열을 객체로 다루며, 이를 통해 문자열 연결 등의 작업을 더 효율적으로 할 수 있게 되었습니다.
2. 대안: 문자열 연결은 C++에서 제공하는 강력한 기능 중 하나지만, 더 복잡한 작업을 위해서는 다른 대안을 고려해볼 수 있습니다. 예를 들어, ```std::stringstream```을 사용하면 더 유연하게 문자열을 처리할 수 있습니다.
3. 구현 세부사항: C++에서는 문자열 연결을 위해 연산자 오버로딩을 사용합니다. 즉, ```+``` 연산자를 사용하여 두 개의 문자열을 연결하면, 내부적으로는 연산자 오버로딩된 함수가 호출되어 작업이 이뤄집니다.

# 관련 자료:

- [C++ 문자열 함수](https://en.cppreference.com/w/cpp/header/cstring)
- [StringStream 클래스](https://www.geeksforgeeks.org/c-stringstream-class-applications/)
- [연산자 오버로딩에 대한 더 자세한 정보](https://www.geeksforgeeks.org/operator-overloading-c/)