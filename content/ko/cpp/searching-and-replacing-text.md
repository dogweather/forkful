---
title:                "텍스트 검색 및 치환하기"
html_title:           "C++: 텍스트 검색 및 치환하기"
simple_title:         "텍스트 검색 및 치환하기"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
텍스트 검색 및 교체는 프로그래머들이 자주 하는 작업입니다. 이는 특정 텍스트를 찾아 다른 텍스트로 대체하는 과정입니다. 프로그래머들은 이를 통해 코드의 일관성을 유지하고 에러를 수정할 수 있습니다.

## 하는 방법:
```C++
// 텍스트 검색 예제
string text = "Hello World!";
string search = "World";

// 검색된 텍스트를 새로운 텍스트로 교체
string replace = "Universe";
replace(text.find(search), search.length(), replace); 

cout << text; // 출력 결과: Hello Universe!
```

```C++
// 텍스트 검색 후 범위 지정 예제
string text = "My favorite color is blue. I also love blueberries.";
string search = "blue";

// 두 번째 blue부터 다음 공백까지를 검색 범위로 지정
replace(text.find(search, text.find(search) + 1), search.length(), "purple"); 

cout << text; // 출력 결과: My favorite color is purple. I also love purpleberries.
```

## 깊이 파고들기:
텍스트 검색 및 교체는 프로그래밍에서 오랜 역사를 가지고 있습니다. 이전에는 이 작업을 수동으로 수행하여 많은 시간과 노력이 필요했습니다. 하지만 지금은 간단한 명령어로 쉽게 할 수 있습니다. 또한, 검색 범위를 지정하여 보다 정확한 결과를 얻을 수도 있습니다.

## 참고:
- [C++ 문자열 함수](https://www.cplusplus.com/reference/string/string/)
- [텍스트 검색 및 교체를 위한 다른 알고리즘](https://www.geeksforgeeks.org/searching-and-replacing-strings-using-a-pattern/)