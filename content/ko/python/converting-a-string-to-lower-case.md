---
title:    "Python: 문자열을 소문자로 변환하기"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 것에 참여하는 이유는 무엇인가요? 문자열은 대소문자를 구분하기 때문에 데이터를 정렬하거나 비교할 때 대소문자를 통일하는 것이 중요합니다. 소문자로 변환하면 데이터 처리가 더 쉬워지고 코드의 가독성이 향상될 수 있습니다.

## 방법

파이썬에서 문자열을 소문자로 변환하는 방법은 매우 쉽습니다. 다음과 같이 간단한 코드를 작성할 수 있습니다.

```Python
string = "Hello World"
lower_string = string.lower()
print(lower_string)
```

실행 결과는 "hello world"가 됩니다. 이처럼 `lower()` 메서드를 사용해 소문자로 변환할 수 있습니다. 

만약 변수에 할당하지 않고 바로 변환된 결과를 출력하고 싶다면 다음과 같이 코드를 작성할 수 있습니다.

```Python
string = "Hello World"
print(string.lower())
```

## 깊이 파헤치기

파이썬에서 문자열을 소문자로 변환하는 방법을 좀 더 자세히 알아보겠습니다. 우선, 파이썬에서는 문자열을 다루는데 유용한 다양한 메서드를 제공하고 있습니다. `lower()` 메서드를 포함하여 `upper()`, `capitalize()`, `title()` 등의 메서드가 있습니다.

`lower()` 메서드는 문자열을 모두 소문자로 변환해주지만 `upper()` 메서드는 모두 대문자로 변환해줍니다. `capitalize()` 메서드는 문자열의 첫 글자만 대문자로 변환해주고 나머지는 소문자로 유지합니다. `title()` 메서드는 단어의 첫 글자를 대문자로 변환해주는데, 단어 구분은 스페이스나 특수 문자 등을 기준으로 합니다.

또한, `lower()` 메서드는 원본 문자열을 변경하지 않고 새로운 문자열을 반환합니다. 따라서, 원본 문자열에는 변화가 없습니다.

## 참고자료

[파이썬 문자열 메서드 공식 문서](https://docs.python.org/3/library/stdtypes.html#string-methods)

[파이썬 문자열 메서드 관련 블로그 포스트](https://realpython.com/python-strings/)

[문자열 관련 유용한 팁과 트릭](https://www.datacamp.com/community/tutorials/python-string-tutorial)

## 더 알아보기