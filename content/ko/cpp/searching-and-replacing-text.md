---
title:    "C++: 텍스트 검색 및 대체"
keywords: ["C++"]
---

{{< edit_this_page >}}

# 왜?

문자열 내의 특정 문자를 찾고 대체하는 작업을 하는 이유는 코드의 유지 및 관리를 위해서입니다. 이 작업은 특정 문자를 일일이 찾아서 바꾸는 대신에 효율적으로 이루어지기 때문에 시간과 노력을 절약할 수 있습니다.

# 방법

C++에서는 string 클래스의 replace() 함수를 사용하여 문자열 내의 특정 문자를 찾고 대체할 수 있습니다. 아래는 이 함수의 예시 코드입니다.

```C++
#include <iostream>
#include <string>

int main() {
    std::string str = "안녕하세요, 여러분!";
    std::cout << "변경 전 문자열: " << str << "\n";

    // "안녕하세요"를 "Hello"로 대체
    str.replace(0, 5, "Hello");
    std::cout << "변경 후 문자열: " << str << "\n";

    return 0;
}
```

위 코드의 출력은 다음과 같을 것입니다.

```
변경 전 문자열: 안녕하세요, 여러분!
변경 후 문자열: Hello, 여러분!
```

위 예시에서는 replace() 함수를 사용하여 문자열의 맨 앞 5개의 문자를 "Hello"로 대체하였습니다. 하지만 더 복잡한 대체 작업을 위해서는 추가적인 매개변수를 사용해야 합니다. 이에 대한 내용은 아래에서 더 자세히 다뤄보겠습니다.

# 깊게 파고들기

문자열의 replace() 함수는 세 가지 매개변수를 가지고 있습니다. 첫 번째 매개변수는 대체를 시작할 인덱스이고 두 번째 매개변수는 대체할 문자의 수입니다. 세 번째 매개변수는 대체할 새로운 문자열입니다. 대체할 문자의 수를 지정하지 않으면 대체할 문자열의 길이만큼 모두 대체하게 됩니다. 또한 string 클래스의 find() 함수를 사용하여 찾을 문자열의 인덱스를 먼저 찾은 후에 replace() 함수를 사용할 수도 있습니다.

더 자세한 내용은 아래 레퍼런스를 참고하시기 바랍니다.

# 관련 링크

- C++ string 클래스 레퍼런스 (http://www.cplusplus.com/reference/string/string/)
- string 클래스 replace() 함수 레퍼런스 (http://www.cplusplus.com/reference/string/string/replace/)
- string 클래스 find() 함수 레퍼런스 (http://www.cplusplus.com/reference/string/string/find/)