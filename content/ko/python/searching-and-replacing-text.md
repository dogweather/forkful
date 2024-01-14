---
title:                "Python: 텍스트 검색 및 대체"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

문자열 검색 및 대체를 하려는 *왜*라는 질문에는 여러 가지 이유가 있습니다. 일반적으로 우리는 대량의 텍스트를 다루는 프로그램을 작성할 때 문자열을 검색하고 대체할 수 있어야 합니다. 예를 들어, 당신은 웹 스크래핑 프로그램을 작성하거나 파일 이름을 일관성 있게 변경하는 것과 같은 작업을 할 때 정규 표현식을 사용하여 문자열을 검색하고 대체할 수 있습니다. 이는 데이터를 정제하고 조작하는 데 유용합니다.

## 어떻게

파이썬에서 문자열을 검색하고 대체하는 방법에 대해 알아보겠습니다. 우선, `re` 라이브러리를 불러올 필요가 있습니다. 그리고 `re.sub()` 함수를 사용하여 패턴을 찾아 원하는 대체 문자열로 바꿀 수 있습니다. 아래의 예제 코드를 살펴보세요.

```python
import re

# 영어에서 'hello'를 '안녕하세요'로 대체하는 함수
def replace_hello(string):
    return re.sub('hello', '안녕하세요', string)

# 예시 문자열
text = 'hello world'

# 함수 실행 및 출력
print(replace_hello(text))
```

출력 결과:

```
안녕하세요 world
```

위의 예시에서는 정규 표현식을 사용하여 모든 'hello'를 '안녕하세요'로 대체했습니다. 단순한 예제지만, 이렇게 문자열 검색과 대체 기능을 사용하여 다양한 작업을 할 수 있습니다.

## 더 알아보기

이제 문자열 검색 및 대체에 대한 기본적인 개념에 대해 알게 되었습니다. 하지만 더 깊이 알아보면 더 많은 기능을 사용할 수 있습니다. 예를 들어, `re.sub()` 함수에는 정규 표현식 옵션을 추가하여 더 정교한 문자열 대체를 할 수 있습니다. 또한 `re.search()` 함수를 사용하여 패턴에 해당하는 문자열을 검색할 수 있습니다. 더 자세한 내용은 아래의 링크들을 참고하세요.

## 관련 링크

- [Python 3 문서 - re.sub() 함수](https://docs.python.org/3/library/re.html#re.sub)
- [Python 3 문서 - re.search() 함수](https://docs.python.org/3/library/re.html#re.search)
- [정규 표현식 튜토리얼 (한국어)](https://wikidocs.net/1642)
- [정규 표현식 테스트 사이트](https://regexr.com/)