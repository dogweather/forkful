---
title:                "문자열을 소문자로 변환하기"
html_title:           "Bash: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇과 왜?
문자열을 소문자로 변환하는 것은 모든 문자의 대문자를 해당하는 소문자로 바꾸는 작업입니다. 프로그래머는 사용자의 입력을 표준화하고, 사용자가 대소문자를 신경 쓸 필요가 없게 만들기 위해 이를 수행합니다.

## 어떻게 하나요?
다음은 Python에서 문자열을 소문자로 변환하는 방법을 보여주는 코드 예제입니다.
```Python
# 문자열 정의
message = "Hello PYTHON World!"

# 소문자 변환
lowercase_message = message.lower()

# 결과 출력
print(lowercase_message)  # 출력: hello python world!
```

## 깊이 들어가보기
문자열을 소문자로 변환하는 것은 오래전부터 자주 쓰이는 기술입니다. Python에서는 내장 함수 `lower()`를 제공하여 이 기능을 쉽게 사용할 수 있도록 지원하고 있습니다. 

대안으로는 문자열의 각 문자를 순회하면서 아스키 코드를 사용해 소문자로 변환하는 등의 저수준 방법이 있지만, 대부분의 경우에는 `lower()` 함수가 훨씬 간편하고 효율적입니다.

소문자 변환 기능의 구현은 언어에 따라 다르지만, Python에서는 유니코드를 지원하여 여러 언어의 문자에 대해서도 소문자 변환을 할 수 있습니다.

## 참고 자료
다음으로 문자열을 소문자로 변환하는 방법에 관한 기타 참고자료들을 링크로 제공합니다:
1. [Python Documentation - String Methods](https://docs.python.org/3/library/stdtypes.html#string-methods)
2. [W3Schools Python String lower() Method](https://www.w3schools.com/python/ref_string_lower.asp)
3. [Stackoverflow - How to convert a string to lower case in Python](https://stackoverflow.com/questions/6797984/how-to-convert-string-to-lower-case-in-python)