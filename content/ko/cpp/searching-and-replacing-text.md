---
title:                "텍스트 검색 및 교체"
html_title:           "Elixir: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 그런가?

문자열에서 텍스트를 찾고 대체하는 것은 흔한 프로그래밍 작업입니다. 이는 데이터 정제, 사용자 입력 처리, 코드 자동화 등 다양한 컨텍스트에서 사용됩니다.

## 어떻게 하는가:

다음은 C++에서 문자열 내의 텍스트를 찾아 대체하는 작업을 수행하는 간단한 코드 예제입니다.

```C++
#include <string>
#include <iostream>

int main(void) {
  std::string s = "Hello, World!";
  size_t pos = s.find("World");

  if(pos != std::string::npos)
     s.replace(pos, 5, "C++");

  std::cout << s << '\n';

  return 0;
}
```

위 코드를 실행하면 결과가 출력됩니다:

```C++
Hello, C++!
```
그래서 "World"라는 단어가 "C++"로 대체되었습니다.

## 깊은 이해

텍스트 탐색 및 대체 기능은 거의 모든 프로그래밍 언어에서 제공되며, 특별히 C++은 STL(Standard Template Library) 내의 문자열에서 직접 사용할 수 있습니다.

대체 방법으로, C++에서 Boost Library의 `replace_all`함수를 사용할 수도 있습니다.

```C++
#include <boost/algorithm/string/replace.hpp>
boost::replace_all(s, "World", "C++");
```

하지만 이 방법은 부스트 라이브러리가 설치되어 있어야 합니다.

## 참조

아래 링크에서 C++ 텍스트 검색 및 대체에 대한 더 많은 정보를 찾을 수 있습니다:

1. [cplusplus.com: std::string - find](http://www.cplusplus.com/reference/string/string/find/)
2. [cplusplus.com: std::string - replace](http://www.cplusplus.com/reference/string/string/replace/)
3. [Boost Library: replace_all function](https://www.boost.org/doc/libs/1_73_0/doc/html/string_algo/usage.html#id-1.3.3.6.9)