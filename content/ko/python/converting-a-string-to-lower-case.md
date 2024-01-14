---
title:    "Python: 문자열을 소문자로 변환하기"
keywords: ["Python"]
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 바꾸는 것이 왜 중요한지 이유는 문자열을 비교하거나 검색할 때 대소문자를 무시하기 위해서입니다.

## 어떻게

```python
# 기본적인 문자열을 소문자로 바꾸는 방법
string = "Hello World"
lower_case = string.lower()
print(lower_case)

# 리스트 안에 있는 문자열들을 모두 소문자로 바꾸는 방법
string_list = ["Apple", "Banana", "Orange"]
lower_case_list = [x.lower() for x in string_list]
print(lower_case_list)

```

출력:

```
hello world
['apple', 'banana', 'orange']
```

## 깊이 파고들기

파이썬에서 문자열을 소문자로 바꾸는 방법은 `lower()` 함수를 사용하는 것입니다. 이 함수는 문자열 객체의 메서드이기 때문에 문자열에 바로 적용할 수 있습니다. 또한 `lower()` 함수는 문자열에 포함된 모든 문자를 소문자로 바꿔줍니다.

그러나 주의해야 할 점이 있습니다. `lower()` 함수는 문자열 객체를 변경하지 않고 새로운 문자열 객체를 반환합니다. 따라서 원본 문자열을 바꾸고 싶으면 다시 변수에 할당해주어야 합니다.

또한 `lower()` 함수는 영어에만 적용되는 것이 아니라, 사용되는 언어의 규칙에 따라 적용되는 것이기 때문에 다른 언어의 경우에는 원하는 결과를 얻을 수 없을 수도 있습니다.

## 아래 링크들을 참고하세요

[파이썬 문자열 관련 문서](https://docs.python.org/3/library/stdtypes.html#string-methods) 
[다른 언어에서의 문자열 비교](https://stackoverflow.com/questions/319426/how-do-i-do-a-case-insensitive-string-comparison)