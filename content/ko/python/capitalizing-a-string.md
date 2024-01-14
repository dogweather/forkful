---
title:    "Python: 문자열 대문자로 바꾸기"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜
문자열을 대문자로 바꾸는 것에 참여하는 이유는 여러 가지가 있을 수 있습니다. 예를 들어, 문자열이 데이터베이스나 파일에 저장될 때 대문자로 저장하는 것이 일반적이기 때문입니다. 또는 사용자의 입력을 대문자로 바꾸어서 처리하는 경우도 있을 수 있습니다.

## 어떻게
파이썬에서 문자열을 대문자로 바꾸는 방법은 매우 간단합니다. 먼저, 문자열 변수를 선언하고 그 값을 할당합니다. 그리고 `upper()` 함수를 사용하여 대문자로 변환할 수 있습니다. 아래 예제를 확인해보세요.

```python
# 변수에 문자열 값 할당
string = "hello world"

# 대문자로 변환하기
string = string.upper()

# 변환된 값을 출력
print(string) # 출력 결과: HELLO WORLD
```

위 예제에서는 `upper()` 함수를 사용하여 문자열을 대문자로 변환하고 그 변환된 값을 출력했습니다. 이렇게 간단하게 대문자로 변환할 수 있습니다.

## 심층 분석
파이썬에서 문자열을 대문자로 변환하는 방식을 좀 더 자세히 알아보겠습니다. 파이썬에서는 문자열 변수 뿐만 아니라 `upper()` 함수를 사용하여 문자열을 대문자로 변환할 수 있습니다. 이 함수는 문자열에 포함된 모든 문자를 대문자로 변환합니다. 또한, `lower()` 함수를 사용하면 문자열을 소문자로 변환할 수도 있습니다.

또한, 파이썬에서는 `capitalize()` 함수를 사용하여 문자열의 첫 글자를 대문자로 만드는 것도 가능합니다. 이 함수는 문자열의 첫 글자를 대문자로 만들고 나머지 문자는 소문자로 만듭니다. 이렇게 파이썬에서는 다양한 함수를 사용하여 문자열을 대소문자를 변환할 수 있습니다.

## 참고 자료
- [파이썬 문자열 변환 방법](https://wikidocs.net/21942)
- [파이썬 공식 문서 - 문자열 메소드](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [파이썬 문자열 관련 실습](https://www.w3schools.com/python/exercise.asp?filename=exercise_strings1)