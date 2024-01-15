---
title:                "하위 문자열 추출하기"
html_title:           "Python: 하위 문자열 추출하기"
simple_title:         "하위 문자열 추출하기"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

문자열의 길이가 긴 경우 전체 문자열을 다루는 것은 매우 어렵기 때문에, 문자열에서 원하는 부분만 추출하는 것이 유용합니다.

## 하면서 배우는 방법

### 기본적인 Substring 추출 방법

```Python
string = "오늘은 파이썬을 배우는 날입니다."
substring = string[3:9]

print(substring)
```

결과:

```
파이썬을
```

위의 예시에서 `string` 변수는 추출하고자 하는 부분이 포함된 전체 문자열을 나타냅니다. `substring` 변수에는 전체 문자열에서 추출하고 싶은 부분의 시작 인덱스와 끝 인덱스를 지정해줍니다. 그리고 `print` 함수를 통해 `substring` 변수를 출력하면, 원하는 부분만 추출할 수 있습니다.

### 부분 문자열 추출 및 합치기

```Python
string = "Hi, my name is Python."
greeting = string[:3]
name = string[3:19]
language = string[19:]

print("안녕하세요!" + greeting + "저의 이름은 " + name + "입니다. " + language + "를 배우는 것을 즐기고 있습니다.")
```

결과:

```
안녕하세요!Hi, 저의 이름은 my name is 입니다. Python.를 배우는 것을 즐기고 있습니다.
```

위의 예시에서 각각의 부분 문자열을 추출하고 나서 다시 합쳐서 출력하는 방법을 알 수 있습니다. 이렇게 하면 원하는 부분만 쉽게 추출하고 더 많은 처리를 할 수 있습니다.

## 더 깊게 알아보기

### 문자열 인덱싱과 슬라이싱

파이썬에서 문자열은 배열과 비슷하게 저장되기 때문에, 문자열의 각 문자는 인덱스를 통해 접근할 수 있습니다. 이때 인덱스는 0부터 시작하며, 대괄호 안에 인덱스를 입력하면 해당 문자에 접근할 수 있습니다.

문자열 인덱싱 예시:

```Python
string = "Python"
print(string[0])
```

결과:

```
P
```

슬라이싱은 인덱싱과 비슷하지만, `:`를 이용하여 시작 인덱스와 끝 인덱스를 지정합니다. 끝 인덱스는 실제로 추출하고 싶은 부분 전까지의 인덱스를 의미합니다.

문자열 슬라이싱 예시:

```Python
string = "Python"
print(string[0:4])
```

결과:

```
Pyth
```

### 다른 Substring 처리 방법

위에서는 문자열을 인덱싱하고 슬라이싱함으로써 부분 문자열을 추출하는 방법을 알아보았습니다. 하지만 파이썬에서는 `split()` 메소드와 정규표현식을 활용하여 더 다양한 방법으로 문자열을 처리할 수 있습니다.

`split()` 메소드는 문자열을 특정 구분자를 기준으로 나누어서 리스트로 반환해줍니다. 정규표현식은 복잡한 규칙에 따라 문자열을 추출하고 처리하는 방법입니다.

더 자세한 내용은 아래의 "참고 자료"를 확인해보세요!

## 참고 자료

- [GeeksforGeeks: String manipulation in Python](https://www.geeksforgeeks.org/string-manipulation-python-set-1/)
- [Real Python: Working with Strings in Python](https://realpython.com/python-strings/)
- [W3Schools: Python Strings](https://