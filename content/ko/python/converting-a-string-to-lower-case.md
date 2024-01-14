---
title:                "Python: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 작업은 대소문자를 구분하지 않는 검색 기능이나 비교 작업을 수행할 때 유용합니다.

## 어떻게

```Python
# 문자열 변수 선언
sentence = "안녕하세요, PYTHON 입니다."

# 소문자로 변환
lowercase = sentence.lower()

# 출력
print(lowercase) # 출력 결과: "안녕하세요, python 입니다."
```

## 깊게 들어가기

문자열을 소문자로 변환하는 방법에는 여러 가지가 있지만, 가장 간단하고 빠른 방법은 `.lower()` 메소드를 사용하는 것입니다. 이 메소드는 문자열을 모두 소문자로 변환해주며, 이를 새로운 변수에 저장하거나 바로 출력할 수 있습니다. 또 다른 방법으로는 `str.casefold()` 메소드를 사용하는 것도 가능합니다. 이 메소드는 `.lower()` 메소드와 비슷한 기능을 수행하지만, 유니코드 문자를 더 잘 처리할 수 있습니다. 

## 참고 자료

- [Python 공식 문서 - str.lower() 메소드](https://docs.python.org/3/library/stdtypes.html#str.lower)
- [Python 공식 문서 - str.casefold() 메소드](https://docs.python.org/3/library/stdtypes.html#str.casefold)
- [w3schools.com - Python Strings](https://www.w3schools.com/python/python_strings.asp)