---
title:                "문자열을 소문자로 변환하기"
html_title:           "Python: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 무엇이며 왜?

문자열을 소문자로 변환하는 것은 아주 간단한 작업입니다. 프로그래머들은 이를 수행하는 이유는 글자들을 통일된 형식으로 표현하기 위해서입니다. 대소문자를 구분하지 않는 경우, 대소문자가 다른 문자열을 비교하는 것은 매우 복잡하고 오류가 발생할 수 있기 때문입니다.

## 어떻게?

Python에서 문자열을 소문자로 변환하는 방법은 상당히 간단합니다. 문자열을 사용한 다음 `lower()` 메소드를 호출하면 됩니다. 아래에는 예제 코드와 출력 결과가 나와 있습니다.

```Python
text = "Hello World"
print(text.lower())
# 출력 결과: hello world
```

## 깊이 들어가보기

소문자 변환은 예전부터 프로그래밍 언어에서 사용되어 왔습니다. 예를 들어, C언어에서는 `tolower()` 함수를 사용하고, Java에서는 `toLowerCase()` 메소드를 사용합니다. 또한, 정규 표현식을 사용하여 문자열을 소문자로 변환할 수도 있습니다.

## 관련 자료

- [Python 메소드: 문자열과 리스트](https://docs.python.org/3/tutorial/introduction.html#strings-and-lists)
- [Python 공식 문서: 문자열 메소드](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [정규 표현식을 사용한 문자열 변환](https://www.techbeamers.com/python-regex/)