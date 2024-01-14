---
title:                "Python: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜?

문자열의 길이를 찾는 활동에 참여하는 이유는 무엇일까요? 문자열은 프로그래밍에서 매우 중요한 데이터 타입이기 때문입니다. 따라서 문자열을 다루는 능력은 프로그래머로서 필수적인 기술입니다.

## 방법

문자열의 길이는 Python의 `len()` 함수를 사용하여 쉽게 찾을 수 있습니다. 예를 들어, 다음과 같은 코드를 사용하면 됩니다.

```Python
string = "안녕하세요"
length = len(string)
print(length)
```

출력 결과는 다음과 같을 것입니다.

```
5
```

## 심층 탐구

`len()` 함수는 문자열의 길이를 찾는 데 사용됩니다. 이 함수는 입력으로 문자열을 받고, 해당 문자열의 길이를 정수 형태로 반환합니다. 이 외에도 `len()` 함수는 리스트, 튜플, 딕셔너리 등의 다양한 데이터 구조에서 길이를 측정할 수 있습니다.

또한 `len()` 함수는 문자열의 공백도 문자로 취급하여 공백의 개수도 길이로 계산합니다. 따라서 이 함수를 사용할 때는 이러한 점을 유의해야 합니다.

## 같이 보기

- [Python의 문자열 관련 함수들](https://www.programiz.com/python-programming/string)
- [Python에서 문자열 다루는 방법](https://realpython.com/python-strings/)
- [Python 문자열 포맷팅 공식 문서](https://docs.python.org/3/library/string.html#format-string-syntax)