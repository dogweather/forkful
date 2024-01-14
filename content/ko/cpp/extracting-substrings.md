---
title:    "C++: 부분 문자열 추출"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜
문자열에서 부분 문자열을 추출하는 것에 관심이 있을까요?

부분 문자열 추출은 굉장히 유용한 기술입니다. 예를 들어, 특정 문자열에서 원하는 정보를 추출하고 다른 문자열과 결합하여 원하는 출력을 만들 수 있습니다.

## 방법
부분 문자열을 추출하는 방법은 다양한 방법이 있지만, 여기서는 C++을 사용하여 예를 들어보겠습니다.

### C++ 코드 예시:
```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // 첫 번째 예시: 문자열에서 특정 인덱스부터 지정된 길이의 부분 문자열 추출
    string str1 = "안녕하세요";
    string sub1 = str1.substr(0, 2); // 변수 sub1에는 "안녕"이 저장됩니다.

    // 두 번째 예시: 특정 인덱스부터 끝까지 부분 문자열 추출
    string str2 = "Hello World";
    string sub2 = str2.substr(6); // 변수 sub2에는 "World"가 저장됩니다.

    // 세 번째 예시: 특정 문자를 기준으로 부분 문자열 추출
    string str3 = "Hello/World";
    size_t pos = str3.find("/"); // 문자열에서 '/'가 처음 나오는 인덱스를 찾아 변수 pos에 저장
    string sub3 = str3.substr(pos + 1); // 변수 sub3에는 "World"가 저장됩니다.

    // 위 예시들은 모두 출력 결과가 같습니다.
    cout << sub1 << endl; // 출력: 안녕
    cout << sub2 << endl; // 출력: World
    cout << sub3 << endl; // 출력: World

    return 0;
}
```

## 깊이있는 이해
부분 문자열을 추출하는 방법은 string 클래스의 멤버 함수인 substr()을 사용하면 쉽게 구현할 수 있습니다. 이 함수는 두 개의 매개변수를 가지는데, 첫 번째 매개변수는 부분 문자열을 추출할 시작 인덱스이고, 두 번째 매개변수는 추출할 문자열의 길이입니다. 만약 두 번째 매개변수를 생략하면, 시작 인덱스부터 끝까지의 문자열이 추출됩니다.

또한, find() 함수를 사용하여 특정 문자나 문자열이 처음으로 나타나는 인덱스를 찾을 수 있습니다. 이를 활용하면 특정 조건에 따라 부분 문자열을 추출하는 것도 가능합니다.

## 더 알아보기
- [C++ string 클래스의 substr() 함수 문서](https://www.cplusplus.com/reference/string/string/substr/)
- [string 클래스의 find() 함수 문서](https://www.cplusplus.com/reference/string/string/find/)
- [GeeksforGeeks: C++에서 부분 문자열 추출하기](https://www.geeksforgeeks.org/cpp-program-extracting-specified-characters-from-string/)