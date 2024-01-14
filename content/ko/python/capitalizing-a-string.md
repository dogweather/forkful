---
title:                "Python: 문자열 대문자로 변환하기"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜
문자열을 대문자로 바꾸는 것에 대해 이야기해보려고 합니다. 이것은 프로그래밍에서 매우 일반적인 작업이며, 문자열의 검색, 정렬, 비교 등 여러 가지 이유로 인해 문자열이 대문자로 표시되어야 할 수 있습니다.

## 어떻게
파이썬에서 문자열을 대문자로 바꾸는 방법에 대해 알아보겠습니다. 아래의 예제 코드와 함께 실제 출력도 함께 확인해보세요.

```Python
# 문자열을 변수에 할당합니다.
word = "hello world"

# 문자열을 대문자로 바꿉니다.
capitalized_word = word.upper()

# 결과를 출력합니다.
print(capitalized_word)
```

실행 결과:
```
HELLO WORLD
```

위의 예제 코드에서 `upper()` 메소드를 사용하여 문자열을 대문자로 바꾸는 것을 확인할 수 있습니다. 이외에도 `capitalize()` 메소드를 사용하여 첫 글자만 대문자로 바꾸거나, `title()` 메소드를 사용하여 모든 단어의 첫 글자를 대문자로 바꿀 수도 있습니다.

## 더 깊게
파이썬에서 문자열을 처리하는 방법에 대해 더 자세히 알아보고 싶다면 다음 링크들을 참고해보세요.

[공식 파이썬 문자열 메소드 문서](https://docs.python.org/3/library/stdtypes.html#string-methods)
[TutorialsPoint의 파이썬 문자열 처리 튜토리얼](https://www.tutorialspoint.com/python/string_upper.htm)
[Real Python의 파이썬 문자열 처리 가이드](https://realpython.com/python-strings/)

## 관련 링크
[공식 파이썬 문서](https://www.python.org/)
[프로그래밍 언어 비교: 파이썬 vs. 자바](https://dev.to/mandeepbhutani/a-comparative-study-of-python-and-java-overview-5689)