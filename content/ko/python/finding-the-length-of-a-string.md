---
title:                "문자열의 길이 찾기"
html_title:           "Lua: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇이고 왜 사용하는가?

문자열의 길이 찾기는 문자열에 얼마나 많은 문자들이 포함되어 있는지를 파악하는 작업입니다. 이는 배열이나 리스트에 데이터를 처리하거나 검증 작업을 수행할 때 중요한 역할을 합니다.

## 어떻게 사용하는가:

```Python
# 문자열 선언
str = "안녕하세요, 파이썬입니다!"
# len함수를 이용하여 문자열 길이 찾기
print(len(str)) 
```
위의 코드를 실행하면 결과는 '13'이 출력됩니다. 이것은 문자열에 13개의 문자가 있다는 것을 알려줍니다.

## 깊게 살펴보기:

사실 문자열의 길이를 측정하는 것은 컴퓨터 프로그래밍의 역사와 함께 해왔습니다. 그리고 파이썬에서는 `len()`라는 내장함수를 이용해서 이 작업을 처리합니다. 그 외에도 문자열을 순회하면서 수를 증가시켜서 길이를 측정할 수도 있지만, `len()` 함수가 가장 효과적일 뿐만 아니라 코드가 짧아서 가장 많이 사용됩니다.

```Python
# 반복문을 사용하여 문자열 길이 찾기
str = "안녕하세요, 파이썬입니다!"
length = 0
for char in str:
    length += 1
print(length)
```
위의 코드를 실행하면 `len()` 함수와 마찬가지로 '13'이 출력됩니다.

## 추가 정보:

다음 링크는 문자열에 대한 더 깊은 이해와 문자열의 길이를 찾는 방법에 대한 더 많은 정보를 제공합니다:

1. 파이썬 공식 문서의 문자열 메소드: <https://docs.python.org/3/library/stdtypes.html#string-methods>
2. 문자열의 길이 찾는 방법에 대한 stackoverflow 토론: <https://stackoverflow.com/questions/17332759/tuples-and-strings>
3. 파이썬에서의 문자열 처리 방식: <https://realpython.com/python-strings/>
4. 파이썬 문자열 함수: <https://www.w3schools.com/python/python_ref_string.asp>