---
title:                "문자열 소문자로 변환하기"
html_title:           "C++: 문자열 소문자로 변환하기"
simple_title:         "문자열 소문자로 변환하기"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜
문자열을 소문자로 변환하는 것이 왜 중요한지 설명하는 2문장.

문자열의 대소문자는 프로그래밍에서 매우 중요합니다. 일관성 있는 데이터 처리와 문자열 비교를 위해 다른 사람이 작성한 코드를 이해하거나 본인이 작성한 코드를 유지보수할 때 대소문자를 일치시키는 것이 매우 중요합니다. 따라서 문자열을 소문자로 변환하는 것은 프로그래머에게 매우 유용합니다.

## 하는 법
변환 함수를 사용하여 문자열을 소문자로 변환하는 예시 코드와 출력 결과값이 포함된 "```C++ ... ```" 코드 블록.

```C++
#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

int main() {
    string words = "HeLLo WOrLd!";
    transform(words.begin(), words.end(), words.begin(), ::tolower);
    cout << words << endl;
    return 0;
}

// 출력 결과: hello world!
```

위의 예시 코드에서는 "transform" 함수를 사용하여 문자열의 각 문자를 소문자로 변환하였습니다. "words.begin()", "words.end()" 매개변수는 문자열의 첫번째와 마지막 문자를 가리키는 포인터입니다. "::tolower"는 문자를 소문자로 변환하는 함수를 의미합니다.

## 더 알아보기
소문자 변환에 대한 자세한 정보를 제공하는 부가 설명.

"transform" 함수는 C++ 표준 라이브러리에서 제공하는 함수이며 문자열을 다루는 "algorithm" 라이브러리에 포함되어 있습니다. 이 함수는 우리가 사용한 예시와 같이 원본 문자열을 변경하지 않고 새로운 문자열을 생성하기 때문에 보통 새로운 변수를 사용해야 합니다.

또한, C++11 이상에서는 "std::transform" 함수를 사용하면 매개변수로 람다 함수를 전달하여 더 간단하게 소문자 변환을 할 수 있습니다.

## 참고 자료
- [C++ 문자열 변환 방법](https://copynull.tistory.com/7)
- [C++ 표준 라이브러리 문서](https://en.cppreference.com/w/)