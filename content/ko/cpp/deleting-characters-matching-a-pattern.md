---
title:    "C++: 패턴과 일치하는 문자 삭제하기"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 왜

문자 패턴과 일치하는 문자를 삭제하는 것이 유용한 이유는 다양합니다. 예를 들어, 동일한 패턴의 문자가 반복적으로 나타나는 경우, 이를 일괄적으로 삭제하면 더 깔끔한 코드를 작성할 수 있습니다. 또한, 특정 데이터에서 불필요한 문자를 제거하여 데이터의 정확성을 높이는데 도움이 됩니다.

# 어떻게

문자를 삭제하는 방법은 간단합니다. 올바른 패턴을 지정하고 해당 패턴에 일치하는 문자를 삭제하면 됩니다. 아래 예제를 참고해 보세요.

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    // 입력 받은 문자열
    string input = "안녕하세요! Hello! こんにちは！";
    // 삭제할 문자 패턴
    string pattern = "!";

    // 입력 문자열을 순회하면서 패턴과 일치하는 문자 삭제
    for (int i = 0; i < input.length(); i++) {
        if (input.substr(i, 1) == pattern) {
            input.erase(i, 1);
            i--;
        }
    }

    // 결과 출력
    cout << input << endl;
    // 출력 결과: 안녕하세요 Hello こんにちは
    return 0;
}
```

### 샘플 출력
입력 문자열: 안녕하세요! Hello! こんにちは！
삭제할 패턴: !

결과: 안녕하세요 Hello こんにちは

# 딥 다이브

문자를 삭제하는 방법은 다양합니다. 위 예제에서는 for 루프를 이용하여 일치하는 문자를 삭제했지만, 정규식을 사용하면 더 빠르고 간편하게 문자를 삭제할 수 있습니다. 또한, 여러 가지 문자 패턴을 동시에 삭제할 수도 있습니다. 더 자세한 내용은 다음 링크를 참고하세요.

# 참고

- [C++ 문자열 삭제 예제](https://www.geeksforgeeks.org/erase-remove-idioms-c/)
- [C++ 정규식 사용하기](http://www.cplusplus.com/reference/regex/)
- [C++ string 클래스의 erase() 함수](http://www.cplusplus.com/reference/string/string/erase/)