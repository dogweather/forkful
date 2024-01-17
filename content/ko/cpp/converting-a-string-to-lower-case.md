---
title:                "대소문자로 변환하기"
html_title:           "C++: 대소문자로 변환하기"
simple_title:         "대소문자로 변환하기"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

각각의 문자열을 소문자로 변환하는 것을 말합니다. 프로그래머들은 주로 입력값을 소문자로 통일하여 일관성 있게 처리하기 위해 이 작업을 수행합니다.

## 방법:

### 예시 1: 문자열 변환 함수 사용

```C++
#include <iostream>
#include <string>

using namespace std;

// 입력값을 소문자로 변환하는 함수
string toLower(string input) {
    for (int i = 0; i < input.length(); i++) {
        // 각 문자를 소문자로 변환
        input[i] = tolower(input[i]);
    }
    return input;
}

int main() {
    string input = "HeLlO WoRlD";
    string result = toLower(input);
    
    // 결과 출력
    cout << result << endl;
    
    return 0;
}

```

**출력:** "hello world"

## 깊은 들어가기:

### 역사적 배경:

옛날에는 대소문자를 구분하는 시스템과 프로그래밍 언어가 많았기 때문에 문자열을 소문자로 변환하는 과정은 더욱 중요했습니다. 하지만 현재 대부분의 프로그래밍 언어는 대소문자를 구분하지 않기 때문에 문자열을 소문자로 변환하는 과정은 선택적이 될 수 있습니다.

### 대체 방법:

문자열을 소문자로 변환하는 방법은 다양합니다. 위 예시에서는 각 문자를 직접 소문자로 변환하는 방법을 사용했지만, 더 효율적이고 간단한 방법도 존재합니다. 예를 들어, C++의 표준 라이브러리 함수 중 하나인 `transform`을 사용하면 간단하게 문자열을 소문자로 변환할 수 있습니다.

### 구현 세부 정보:

문자열을 소문자로 변환하는 구현 방법은 다양합니다. 예를 들어, ASCII 코드를 사용하거나 `tolower`과 같은 라이브러리 함수를 사용할 수 있습니다. 또한, 유니코드를 지원하는 언어에서는 다양한 문자 세트를 고려하여 문자열을 변환해야 할 수도 있습니다. 이를 염두에 두고 구현을 선택해야 합니다.

## 관련 사이트:

- [C++ `tolower` 함수 문서](https://www.cplusplus.com/reference/cctype/tolower/)
- [C++ `transform` 함수 문서](https://www.cplusplus.com/reference/algorithm/transform/)