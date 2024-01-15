---
title:                "문자열 소문자로 변환하기"
html_title:           "Python: 문자열 소문자로 변환하기"
simple_title:         "문자열 소문자로 변환하기"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜 

본문에서 소문자로 변환하는 것이 중요한 이유는 프로그래밍에서 텍스트를 처리할 때 대소문자를 구분하기 때문입니다.

## 어떻게 

```Python
text = "Hello, World!"
lowercase_text = text.lower()
print(lowercase_text)
```

**결과:**
hello, world!

문자열의 `lower()` 함수를 사용하여 간단하게 문자열을 소문자로 변환할 수 있습니다.

## 깊게 들어가보기

파이썬에서 문자열은 변경할 수 없는(immutable) 자료형입니다. 따라서 문자열을 직접 수정하는 것은 불가능합니다. 하지만 `lower()` 함수는 원래의 문자열을 변경하는 것이 아니라 새로운 소문자로 된 문자열을 반환합니다. 따라서 변수에 할당하여 사용하는 것이 중요합니다.

또한 `lower()` 함수는 유니코드 문자열에 대해서도 작동합니다. 이는 파이썬의 유니코드 지원이 우수하기 때문에 다양한 언어의 문자열도 쉽게 소문자로 변환할 수 있다는 것을 의미합니다.

## 관련 정보

[파이썬 문자열 메서드](https://docs.python.org/3/library/stdtypes.html#string-methods)

[파이썬 유니코드 문자열 처리](https://docs.python.org/3/howto/unicode.html)