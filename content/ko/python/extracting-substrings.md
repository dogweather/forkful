---
title:                "부분 문자열 추출"
html_title:           "Arduino: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

서브스트링 추출은 문자열의 특정 부분을 선택하는 작업입니다. 프로그래머들은 이를 활용하여 데이터를 정제하거나 분석에 필요한 정보만을 따로 추출할 때 사용합니다.

## 사용 방법:

```Python
s = "Hello, World!"
print(s[7:12])  # 출력: World
```
위의 예제에서, 우리는 문자열에서 "World"라는 부분문자열을 추출했습니다. 인덱스 7에서 12까지의 문자들을 선택 했습니다.

```Python
s = "안녕하세요, 세상!"
print(s[7:9])  # 출력: 세상
```
이 예제에서는 한글을 사용한다는 것을 제외하면 위의 영문 예제와 같은 원칙이 적용됩니다.

## 깊이 있게 알아보기:

서브스트링 추출은 프로그래밍의 기능 중 하나로, 많은 알고리즘이 이 기능을 사용합니다. 종종 데이터를 분석하거나 텍스트 검색을 수행할 때 이 기능이 필요합니다.

대안적인 방법으로는 Python의 `split()` 함수를 사용할 수 있습니다. 이 함수는 문자열을 나누어 리스트로 반환하고, 여러분은 이 리스트에서 필요한 원소를 선택할 수 있습니다.

```Python
s = "Hello, World!"
print(s.split(", ")[1])  # 출력: World!
```
물론, 이 방법은 추출하고 싶은 부분이 명확한 구분자로 분리되어 있어야만 유용합니다.

파이썬에서 문자열의 인덱스는 0부터 시작하며, 첫 문자의 인덱스는 0이고, 두번째 문자의 인덱스는 1입니다. 이런 세부사항이 서브스트링을 추출할 때 중요합니다.

## 참고자료:

- 파이썬 공식 문서(string): https://docs.python.org/ko/3/library/string.html
- 파이썬 튜토리얼(문자열 메서드): https://docs.python.org/ko/3/tutorial/introduction.html#strings
- W3Schools Python 문자열 해설: https://www.w3schools.com/python/python_strings.asp