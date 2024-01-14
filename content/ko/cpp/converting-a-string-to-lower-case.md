---
title:    "C++: 문자열 대문자를 소문자로 변환하기"
keywords: ["C++"]
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 것에 대해 생각해본 적이 있나요? 그 이유는 간단합니다. 문자열을 처리해야 할 때, 대문자와 소문자를 구별하지 않고 비교하고 싶을 때가 있기 때문입니다.

## 어떻게 하나요

```C++
#include <iostream> 
#include <string> 
#include <cctype> 

using namespace std; 

int main() { 
    string input; 
    cout << "문자열을 입력하세요: "; 
    getline(cin, input); 

    for (int i = 0; i < input.length(); i++) { 
        input[i] = tolower(input[i]); 
    } 

    cout << "소문자로 변환된 문자열: " << input << endl; 

    return 0; 
} 
```

**출력 결과:** 

```
문자열을 입력하세요: Hello World 
소문자로 변환된 문자열: hello world
```

## 깊이있게 살펴보기

위 코드에서, `cctype` 헤더 파일의 `tolower()` 함수를 사용하여 대문자를 소문자로 변환하는 방법을 살펴보았습니다. 이 함수는 주어진 문자를 처리하기 전에 대문자인지 확인하고, 대문자라면 이를 소문자로 변환합니다. 이렇게 하면 문자열의 모든 문자를 하나씩 확인하면서 소문자로 변환할 수 있습니다.

## 더 알아보기

문자열을 소문자로 변환하는 방법은 `cctype` 헤더 파일에 더 많은 함수들이 있는 것을 알고 계신가요? 예를 들어, `toupper()` 함수를 사용하면 문자열을 대문자로 변환할 수도 있습니다. 이외에도 `ispunct()`, `isalpha()`, `isdigit()` 등 다양한 함수들이 문자열을 처리하고 검사하는 데 유용하게 사용될 수 있습니다.

## 더 많은 정보 보기

[cppreference.com](https://ko.cppreference.com) - C++ 문서와 예제를 제공하는 공식 사이트입니다.

[C++ 코딩 가이드](http://naver.me/xBdMTYfK) - 한국어로 된 C++ 코딩 가이드입니다.

See Also (관련 자료):

- [이 글의 GitHub 리포지토리](https://github.com/jiindev/blog-articles)