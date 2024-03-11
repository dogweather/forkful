---
date: 2024-01-20 17:41:44.442884-07:00
description: "\uBB38\uC790 \uD328\uD134\uC744 \uC0AD\uC81C\uD558\uB294 \uAC83\uC740\
  \ \uBB38\uC790\uC5F4\uC5D0\uC11C \uD2B9\uC815 \uADDC\uCE59\uC774\uB098 \uC870\uAC74\
  \uC744 \uB9CC\uC871\uD558\uB294 \uBD80\uBD84\uC744 \uC81C\uAC70\uD558\uB294 \uCC98\
  \uB9AC \uBC29\uBC95\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uB370\uC774\uD130\uB97C \uC815\uC81C\uD558\uAC70\uB098 \uC6D0\uD558\uB294 \uD615\
  \uC2DD\uC5D0 \uB9DE\uCDB0 \uAC00\uACF5\uD558\uAE30 \uC704\uD574 \uC774 \uC791\uC5C5\
  \uC744 \uC218\uD589\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-11T00:14:29.572669-06:00'
model: gpt-4-1106-preview
summary: "\uBB38\uC790 \uD328\uD134\uC744 \uC0AD\uC81C\uD558\uB294 \uAC83\uC740 \uBB38\
  \uC790\uC5F4\uC5D0\uC11C \uD2B9\uC815 \uADDC\uCE59\uC774\uB098 \uC870\uAC74\uC744\
  \ \uB9CC\uC871\uD558\uB294 \uBD80\uBD84\uC744 \uC81C\uAC70\uD558\uB294 \uCC98\uB9AC\
  \ \uBC29\uBC95\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\
  \uC774\uD130\uB97C \uC815\uC81C\uD558\uAC70\uB098 \uC6D0\uD558\uB294 \uD615\uC2DD\
  \uC5D0 \uB9DE\uCDB0 \uAC00\uACF5\uD558\uAE30 \uC704\uD574 \uC774 \uC791\uC5C5\uC744\
  \ \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uD328\uD134\uC5D0 \uC77C\uCE58\uD558\uB294 \uBB38\uC790 \uC0AD\uC81C"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자 패턴을 삭제하는 것은 문자열에서 특정 규칙이나 조건을 만족하는 부분을 제거하는 처리 방법입니다. 프로그래머들은 데이터를 정제하거나 원하는 형식에 맞춰 가공하기 위해 이 작업을 수행합니다.

## How to: (방법)
C++에서는 `<algorithm>` 헤더에 있는 `erase()` 및 `remove_if()` 함수를 이용해서 문자 패턴을 삭제할 수 있습니다. 다음 예제 코드와 출력을 확인해 보세요.

```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {
    std::string data = "안녕하세요123, C++ 프로그래밍!";

    // 숫자를 제거합니다.
    data.erase(std::remove_if(data.begin(), data.end(), ::isdigit), data.end());
    std::cout << data << std::endl;

    return 0;
}
```
출력:
```
안녕하세요, C++ 프로그래밍!
```

## Deep Dive (심층 탐구)
이전의 C++에서는 문자열 조작을 위해 자체 루프를 작성하는 것이 일반적이었습니다. 하지만, C++11부터는 람다 표현식과 함께 `std::remove_if()` 함수를 사용하여 코드를 간결하게 만들 수 있게 되었습니다. 예를 들어, 람다를 사용하여 위의 예제에서 숫자를 제거하는 코드는 다음과 같습니다:
```C++
data.erase(std::remove_if(data.begin(), data.end(),
                          [](unsigned char c) { return std::isdigit(c); }),
           data.end());
```
`std::remove_if()`는 조건과 매치되는 모든 원소를 sequence의 끝으로 이동시키고, 새 sequence의 끝을 가리키는 iterator를 반환합니다. 그런 다음 `erase()`를 사용하여 매치된 원소를 실제로 제거합니다.

또 다른 대안으로는 정규 표현식을 사용하는 방법이 있습니다. `<regex>` 헤더 파일을 포함시키고 `std::regex`를 사용하여 복잡한 패턴의 문자열도 쉽게 삭제할 수 있습니다.

## See Also (참고 자료)
- `std::remove_if`: https://en.cppreference.com/w/cpp/algorithm/remove
- `std::string::erase`: https://en.cppreference.com/w/cpp/string/basic_string/erase
- C++ 정규 표현식 사용법: https://en.cppreference.com/w/cpp/regex
- 컨테이너와 알고리즘에 대한 C++ 문서: https://en.cppreference.com/w/cpp/container
