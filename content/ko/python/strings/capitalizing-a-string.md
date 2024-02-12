---
title:                "문자열 대문자화"
aliases:
- /ko/python/capitalizing-a-string/
date:                  2024-02-03T19:06:15.184201-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열 대문자화"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이며, 왜 그럴까?
문자열을 대문자화하는 것은 문자열의 첫 글자를 대문자로 변환하고 나머지를 소문자로 변환하는 것을 의미합니다. 이 작업은 입력을 정규화하거나 제목, 이름 등의 가독성을 향상시키기 위해 데이터 처리에서 일반적으로 사용됩니다.

## 어떻게 할까:

### 파이썬의 내장 메소드 사용하기:
파이썬에는 문자열을 쉽게 대문자화할 수 있는 내장 메소드 `.capitalize()`가 있습니다.

```python
my_string = "hello world"
capitalized_string = my_string.capitalize()
print(capitalized_string)
```
**출력:**
```
Hello world
```

### 여러 단어 처리하기:
문자열의 각 단어가 대문자로 시작해야 하는 시나리오(예: 제목)의 경우, `.title()` 메소드를 적용할 수 있습니다.

```python
my_title = "python programming essentials"
title_case = my_title.title()
print(title_case)
```
**출력:**
```
Python Programming Essentials
```

### 타사 라이브러리 사용하기:
파이썬의 표준 라이브러리가 기본 문자열 대문자화에 적합하지만, `textblob`과 같은 라이브러리는 특히 자연어 처리를 위해 더 세분화된 제어를 제공할 수 있습니다.

먼저 `textblob`이 설치되어 있는지 확인하세요:
```bash
pip install textblob
```

그런 다음, 사용 상황에 따라 `textblob`의 대문자화 기능이 다르게 작동할 수 있음을 염두에 두고 문자열을 대문자화하는 데 사용하세요:

```python
from textblob import TextBlob

my_sentence = "this is a test sentence"
blob = TextBlob(my_sentence)
capitalized_blob = TextBlob(blob.string.capitalize())
print(capitalized_blob)
```
**출력:**
```
This is a test sentence
```

`capitalize()`와 `title()` 메소드가 보편적으로 유용하긴 하지만, `textblob`과 같은 라이브러리를 활용하면 특정 응용 프로그램에 대한 추가적인 유연성을 제공할 수 있다는 점을 기억하세요.
