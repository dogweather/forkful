---
title:                "문자열의 길이 찾기"
html_title:           "Python: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜?

문자열의 길이를 찾는 것이 왜 중요한지 궁금하시다면, 여러분이 프로그래밍에서 문자열을 다루는 것이 매우 중요하다는 것을 알아야 합니다. 문자열은 문자나 단어의 나열로 문자열을 효율적으로 다룰 수 있는지 여부가 중요합니다.

## 어떻게?

```Python
# 문자열의 길이 출력하기
string = "Hello, Python!"
print(len(string))

# 문자열 입력 받아 길이 출력하기
string = input("문자열을 입력하세요: ")
print(len(string))
```

출력 결과:
```
13
11
```

## 더 깊이 들어가보기

문자열의 길이를 찾는 것은 코드에서 변수를 다룰 때 매우 유용합니다. 따라서 문자열의 길이를 정확하게 파악하는 것이 중요합니다. 문자열의 길이는 공백을 포함하여 모든 문자와 기호를 포함하는 것에 유의해야 합니다. 또한, 문자열의 길이를 찾는 방법인 `len()` 함수를 잘 이해하고 활용하는 것이 중요합니다.

## 관련 정보

- [Python 공식 문서 - 문자열 처리](https://docs.python.org/3/library/string.html)
- [GeeksforGeeks - Find length of a string in Python](https://www.geeksforgeeks.org/python-program-to-find-the-length-of-a-string/)