---
title:                "문자열 대문자화"
html_title:           "Python: 문자열 대문자화"
simple_title:         "문자열 대문자화"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 그렇게 하는가?

문자열을 대문자화는 글자들을 모두 대문자로 바꾸는 과정을 의미합니다. 프로그래머는 이를 통해 사용자 입력을 표준화하고, 데이터의 일관성을 유지하기 위해 이 작업을 수행합니다.

## 어떻게 하는가:

Python에서는 문자열을 대문자로 바꾸는 것이 매우 간단합니다. `upper()` 메소드를 사용하면 됩니다. 예를 들면,

```Python
text = "hello world"
capitalized_text = text.upper()
print(capitalized_text)
```

이 코드를 실행하면 다음과 같은 결과가 출력됩니다:

```Python
'HELLO WORLD'
```

## 깊은 이해:

문자열을 대문자로 바꾸는 기능이 Python에 첫 도입된 것은 아닙니다. 이는 초기 프로그래밍 언어에서도 볼 수 있는 기능으로, C 언어의 `toupper()` 함수를 생각하면 됩니다.

Python의 `upper()` 메소드의 대안으로 `capitalize()` 메소드가 있습니다. 이는 문자열의 첫 글자만 대문자로 만드는 함수입니다. 예를 들면,

```Python
text = "hello world"
capitalized_text = text.capitalize()
print(capitalized_text)
```

이 코드를 실행하면, 결과는 다음과 같습니다:

```Python
'Hello world'
```

먼저, `upper()` 메소드는 Python의 문자열 클래스의 내장 함수로, 문자열 내의 모든 ASCII 문자를 대문자로 변환합니다. 유니코드 문자열에도 동작합니다만, 대문자 변환이 가능하지 않은 유니코드 문자는 그대로 둡니다.

## 참고할 만한 자료:

- Python 공식 문서의 문자열 메소드 페이지: https://docs.python.org/3/library/stdtypes.html#string-methods
- 같은 주제에 대한 StackOverflow 토론: https://stackoverflow.com/questions/6797984/how-do-i-uppercase-a-string-in-python
- Python 문자열 메소드에 대한 w3schools 가이드: https://www.w3schools.com/python/python_ref_string.asp