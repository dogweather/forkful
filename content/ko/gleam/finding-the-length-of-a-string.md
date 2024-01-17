---
title:                "문자열의 길이 찾기"
html_title:           "Gleam: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# What & Why? 
문자열의 길이를 찾는 것은 프로그래머들이 많이 하는 작업 중 하나입니다. 문자열의 길이를 찾는다는 것은 문자열에 포함된 문자의 개수를 찾는 것을 의미합니다. 이 작업은 프로그래밍에서 많이 사용되며 문자열의 구조를 파악하는 데 유용합니다.

# How to:
```Gleam
// 문자열의 길이를 찾는 방법은 매우 간단합니다.
// 'length' 함수를 사용하면 됩니다.

length("Hello, world!"); // 13
length("안녕하세요"); // 5
```

# Deep Dive:
이 작업은 일반적으로 문자열 데이터를 다루는 작업에서 매우 중요합니다. 예를 들어, 사용자가 입력한 데이터의 길이를 검증하거나 문자열을 처리하는 과정에서 문자열이 실제로 어떻게 구성되어 있는지 파악하는 데 유용합니다.

대부분의 프로그래밍 언어에서는 문자열 길이를 찾는 함수를 제공하고 있으며, Gleam 역시 예외는 아닙니다. 대부분의 경우 'length' 함수를 사용하여 문자열 길이를 찾을 수 있습니다. 하지만 일부 언어에서는 길이를 재는 함수와 같은 다른 함수를 사용하기도 합니다.

# See Also:
- [Gleam 공식 문서](https://gleam.run/documentation/#strings)
- [W3Schools에서의 문자열 길이 찾는 방법](https://www.w3schools.com/jsref/jsref_length_string.asp)
- [스택 오버플로우에서의 유저들의 대화](https://stackoverflow.com/questions/4036969/whats-the-length-of-a-python-string)