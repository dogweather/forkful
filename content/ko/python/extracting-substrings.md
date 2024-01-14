---
title:                "Python: 서브스트링 추출하기"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

잘 쓰이지 않는 문자열을 추출하는 일은 Python 프로그램을 작성할 때 자주 사용될 수 있습니다. 예를 들어, 특정 단어나 문장을 포함하고 있는지 확인하거나, 문자열을 특정 규칙에 맞춰서 잘라내기 위해 사용됩니다.

## 방법

```Python
# 기본적인 문자열 추출 방법
string = "Python 프로그래밍을 배우고 있습니다."

# 인덱스를 이용한 추출
substring1 = string[0:6]
print(substring1) # Python

# 문자열의 특정 단어를 기준으로 추출
substring2 = string.split(" ")[2]
print(substring2) # 배우고

# 인덱스를 이용한 역순 추출
substring3 = string[-8:]
print(substring3) # 있습니다.
```

위 코드 예시에서는 문자열을 추출하기 위해 인덱스를 사용하는 방법과 `split()` 함수를 이용하는 방법을 보여줍니다. 문자열을 잘라내기 위한 규칙이 있다면, 정규표현식을 사용해 추출하는 것도 가능합니다.

## 심층 탐구

문자열 추출은 순간적으로 보면 간단해 보일 수 있지만, 실제로는 문자열의 인코딩, 슬라이싱 방식 등 여러가지 요소가 관여하게 됩니다. 예를 들어, 문자열의 인코딩 방식이 달라진다면 추출하기 위한 인덱스 역시 달라질 수 있습니다. 따라서 문자열 추출을 잘 이해하고 숙련도를 높이는 것이 중요합니다.

## 참고자료

- [Python 공식 문서 - 문자열 추출](https://docs.python.org/ko/3/library/stdtypes.html#text-sequence-type-str)
- [w3schools - Python Strings](https://www.w3schools.com/python/python_strings.asp)
- [짧고 깔끔한 함수를 사용해 잘라내기](https://pythonkim.tistory.com/37)