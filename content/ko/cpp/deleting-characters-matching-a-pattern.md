---
title:                "C++: 패턴과 일치하는 문자 삭제"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

컴퓨터 프로그래밍에서 우리는 간혹 특정한 패턴과 일치하는 문자들을 삭제해야 할 필요가 있습니다. 이런 경우에는 어떤 이유로 우리는 문자를 삭제해야 할까요? 예를 들어, 사용자의 입력을 검증할 때 입력값에 있는 특수문자를 삭제하고 싶거나, 문자열을 정리하고 싶은 등 많은 이유가 있을 수 있습니다.

## 어떻게

우리는 C++의 다양한 기능을 사용하여 문자열에서 패턴과 일치하는 문자들을 삭제할 수 있습니다. 먼저, 정규식을 사용하여 패턴을 지정해야 합니다. C++에서는 <regex> 헤더 파일을 포함시켜서 이 기능을 사용할 수 있습니다. 그 후, 문자열에서 일치하는 부분을 검색하고 삭제할 수 있는 다양한 방법이 있습니다. 아래 코드를 살펴보세요.

```C++
#include <iostream>
#include <string>
#include <regex>

using namespace std;

int main() {
    string text = "Hello, World!";

    regex pattern("[^a-zA-Z0-9]");
    string replacement = "";

    string result = regex_replace(text, pattern, replacement);

    cout << result << endl;

    return 0;
}
```

위 코드에서는 <iostream>과 <string> 헤더 파일을 포함시키고, 'text'라는 변수에 문자열을 저장합니다. 그 후, 패턴과 일치하는 문자들을 탐색하기 위해 정규식을 사용하고, 지정한 패턴에 맞지 않는 문자들을 빈 문자열로 대체하도록 지정합니다. 그리고 <regex> 헤더 파일의 regex_replace 함수를 사용하여 문자열 내에서 패턴과 일치하는 부분을 찾아 대체한 결과를 'result' 변수에 저장합니다. 마지막으로, 출력 결과를 확인하기 위해 'cout'을 사용하여 'result' 변수의 값을 출력합니다.

위 코드의 출력 결과는 "HelloWorld"가 될 것입니다.

## 심층 탐구

문자를 삭제하는 것은 우리가 생각하는 것보다 더 복잡할 수 있습니다. 예를 들어, 특정한 언어에서는 문자를 삭제할 때 해당 문자 뒤의 모든 문자들을 앞으로 당겨야 할 수도 있습니다. 이때는 해당 언어의 문자 인코딩 방식에 따라 다르게 처리해야 하는 경우가 있습니다. 또한, 언어나 문화에 따라 특정 문자를 삭제하는 방식이 다른 경우도 있을 수 있습니다. 따라서, 문자를 삭제하는 방법에 대해 이해하기 전에는 해당 언어나 문화의 규칙을 고려해야 합니다.

## 관련 링크

- [C++정규식 사용법](https://modoocode.com/304)
- [문자열 처리 함수](http://www.cplusplus.com/reference/string/string/)
- [C++문자 encoding 방식](https://kodehelp.com/cpp-stdstring/)
- [C로 하기](https://stackoverflow.com/questions/1726302/removing-spaces-from-a-string-in-c)