---
title:    "Python: 서브스트링 추출하기"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/python/extracting-substrings.md"
---

{{< edit_this_page >}}

# 왜

자신이 작업하고 있는 문자열에서 부분 문자열을 추출하는 것은 매우 유용합니다. 예를 들어, 특정 문자열을 찾기 위해 검색하거나 문자열을 잘라서 사용자의 편의에 따라 새로운 문자열을 만들 수 있습니다.

## 추출하는 방법

```Python
# 원본 문자열 
string = "안녕하세요, 저는 파이썬을 사랑합니다."

# 부분 문자열 "안녕하세요"
substring = string[0:5]

# 출력 결과
print(substring)
# 안녕하세요
```

위의 예시에서 보듯이 Python에서는 문자열의 인덱스를 사용하여 부분 문자열을 추출할 수 있습니다. 또한 특정 패턴을 가지고 있는 문자열을 추출하고 싶을 때는 정규식을 사용할 수도 있습니다.

## 깊이 파고들기

문자열의 인덱스를 사용하여 부분 문자열을 추출하는 방법은 매우 간단하고 직관적입니다. 그러나 사용자의 요구 사항에 따라서도 정규식을 사용하여 보다 복잡한 부분 문자열을 추출하는 것도 가능합니다. 또한 Python에서는 문자열에서 일어날 수 있는 모든 변화나 예외 상황에 대한 내장 메소드들도 제공하고 있습니다.

# 또보기

- [Python 문자열 메소드](https://docs.python.org/3.9/library/stdtypes.html#string-methods)
- [정규식 튜토리얼](https://www.tutorialspoint.com/python/python_reg_expressions.htm)