---
title:                "문자열 대문자로 변환하기"
html_title:           "C++: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 왜 
문자열을 대문자로 변환하는 것에 대해 시도하는 이유는 주로 사용자가 입력한 문자열을 대문자로 바꾸어 다른 함수나 프로그램에서 처리하기 쉽게 하기 위해서입니다.
하지만 자신만의 프로젝트를 만들거나 기존 코드를 수정하는데 있어서도 대소문자를 일관성있게 사용하는 것이 중요한데, 이를 위해서도 문자열을 대문자로 변환하는 것이 유용합니다.

## 어떻게
이제 C++을 사용하여 문자열을 대문자로 변환하는 방법을 살펴보겠습니다. 아래의 예시 코드를 참고해주세요.

```C++
#include <iostream>
#include <string>
#include <cctype>

using namespace std;

int main() {
    // 대문자로 변환할 문자열 입력
    string str;
    cout << "문자열을 입력하세요: ";
    getline(cin, str);

    // 문자열의 각 문자를 대문자로 변환
    for (int i = 0; i < str.length(); ++i) {
        str[i] = toupper(str[i]);
    }

    // 결과 출력
    cout << "대문자로 변환된 문자열: " << str << endl;

    return 0;
}
```

위의 코드를 실행하면 입력한 문자열이 모두 대문자로 변환되어 출력됩니다. 만약 대문자를 소문자로 바꾸고 싶다면 `toupper` 함수를 대신 `tolower` 함수로 바꿔주면 됩니다.

## 깊게 들어가보기
자 그럼 `toupper` 함수에 대해 조금 더 깊게 살펴보도록 하겠습니다. 이 함수는 `<cctype>` 라이브러리에 정의되어 있으며, 해당 문자가 속한 아스키 표의 값과 계산을 통해 대문자로 변환하는 기능을 수행합니다.
또한, `toupper` 함수는 입력값으로 받은 문자가 소문자인지 검사한 후, 소문자일 경우에만 대문자로 변환하고 그 외의 경우에는 그대로 출력합니다.

# 참고 자료
- [cplusplus.com - toupper](https://www.cplusplus.com/reference/cctype/toupper/)
- [HackerRank - String to Uppercase](https://www.hackerrank.com/challenges/whats-in-a-name/problem)
- [GeeksforGeeks - Convert string to upper case in C++](https://www.geeksforgeeks.org/convert-string-upper-case-using-stl-c/)
 
# 참고 문헌
https://ko.wikipedia.org/wiki/ASCII