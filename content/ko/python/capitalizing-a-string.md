---
title:                "문자열 대문자로 변환하기"
html_title:           "Python: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 첫 글자를 대문자로 변환하는 것은 프로그래밍에서 자주 사용되는 작업 중 하나입니다. 이를 통해 데이터를 정렬하거나 보기 쉽게 표시할 수 있습니다.

## 어떻게

```python
my_string = "korean"
print(my_string.capitalize())
```

**출력:** Korean

```python
my_string = "PYTHON"
print(my_string.capitalize())
```

**출력:** Python

위의 예시 코드를 보면, `capitalize()` 함수를 사용하여 문자열의 첫 글자를 대문자로 변환할 수 있습니다. 이렇게 하면 코드의 가독성을 높이고 데이터를 정렬할 때도 더 쉽게 처리할 수 있습니다.

## 깊게 파고들기

`capitalize()` 함수는 문자열의 첫 글자만 대문자로 변환해주는데, 만약 문자열 내의 모든 단어의 첫 글자를 대문자로 바꾸고 싶다면 어떻게 해야 할까요? 이때 사용할 수 있는 함수가 `title()` 함수입니다.

```python
my_string = "this is a python article"
print(my_string.title())
```

**출력:** This Is A Python Article

또한 `lower()` 함수와 `upper()` 함수를 사용하여 각각 모든 글자를 소문자 또는 대문자로 변환할 수도 있습니다. 마지막으로, `split()` 함수를 사용하여 문자열을 단어로 분리한 후, `capitalize()` 함수를 적용할 수도 있습니다.

## 관련 자료

- [Python의 문자열 메서드](https://www.w3schools.com/python/python_strings_methods.asp)
- [온라인 Python 코드 에디터](https://www.online-python.com/)