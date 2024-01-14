---
title:                "C++: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

문자열을 검색하고 바꿀 때는 일반 텍스트 처리 작업에서 가장 자주 사용되는 기술입니다. 문자열을 검색하고 원하는 결과물로 변경할 수 있기 때문에 프로그래밍에서 매우 유용하게 이용될 수 있습니다.

## 하우투

검색 및 대체 기능을 보유한 많은 프로그램이 있지만, C++의 `"string"` 라이브러리는 사용자가 프로그램에서 문자열을 검색하고, 필요에 따라 바꿀 수 있도록 해줍니다. 다음은 간단한 예제 코드와 출력 결과입니다:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  // 문자열을 생성 및 초기화
  string text = "안녕하세요, 이것은 예제입니다!";

  // 문자열 검색
  size_t index = text.find("예제");

  // 검색된 부분을 새로운 문자열로 대체
  text.replace(index, 2, "샘플");

  // 출력 결과
  cout << text << endl;

  return 0;
}
```
출력 결과:
```
안녕하세요, 이것은 샘플입니다!
```

## 딥 다이브

C++의 `"string"` 라이브러리는 문자열을 검색하고 대체하는 데에 유용한 다양한 기능을 제공합니다. `find()` 함수를 사용하여 원하는 문자열의 위치를 찾을 수 있고, `replace()` 함수를 사용하여 해당 위치의 문자열을 다른 문자열로 변경할 수 있습니다. 또한, `substr()` 함수를 사용하여 특정 부분의 문자열만을 추출할 수도 있습니다.

`"regex"` 라이브러리를 사용하면 더욱 강력한 정규식을 이용하여 문자열을 검색하고 대체할 수 있습니다. 이를 활용하여 다양한 패턴을 검색하고 더욱 복잡한 문자열 처리 작업을 수행할 수 있습니다.

## 참고

다음은 관련된 자료들입니다:

- [C++ string 라이브러리](https://ko.wikipedia.org/wiki/C%2B%2B_string_%EB%9D%BC%EC%9D%B4%EB%B8%8C%EB%9D%BC%EB%A6%AC)
- [C++ regex 라이브러리](https://cplusplus.com/reference/regex/)
- [C++ 문자열을 처리하는 방법](https://madarasz.com/codingtips/cpp-string-usage/)