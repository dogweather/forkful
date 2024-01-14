---
title:                "Python: 대문자를 소문자로 변환하기"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜
문자열을 소문자로 변환하는 것이 왜 중요한지 설명하는 1-2 문장입니다.

## 방법
```Python
# 문자열을 작성합니다.
string = "HELLO WORLD"

# 문자열을 소문자로 변환합니다.
lowercase_string = string.lower()

# 변환된 문자열을 출력합니다.
print(lowercase_string)
```
```
hello world
```

## 깊이있게 들어가기
문자열을 소문자로 변환하는 것은 대소문자에 민감한 언어에서 중요합니다. 대문자로 작성된 문자열을 소문자로 변환하면 나중에 문자열을 검색하거나 제어하기가 쉬워집니다. 또한, 대문자와 소문자를 구분하지 않는 경우에도 사용할 수 있습니다.

변환하는 방법에는 두 가지가 있습니다. 첫 번째는 `lower()` 함수를 사용하는 것이고, 두 번째는 `casefold()` 함수를 사용하는 것입니다. `lower()` 함수는 미국 영어를 기준으로 대문자를 소문자로 변환하고, `casefold()` 함수는 유니코드를 기준으로 변환합니다. 대부분의 경우 `lower()` 함수를 사용해도 충분하지만, 자신이 어떤 언어를 다루고 있는지에 따라 다른 함수를 써야 할 수도 있습니다.

## 참고자료
- [Python 문자열 변환](https://www.w3schools.com/python/ref_string_lower.asp)
- [영어 대소문자를 구분하지 않는 검색](https://www.researchgate.net/publication/5702294_Case_Sensitivity_in_Internet_Search_Evaluating_Comprising_Query_Term_Weighting_Schemes)