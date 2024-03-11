---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:15.184201-07:00
description: "\uBB38\uC790\uC5F4\uC744 \uB300\uBB38\uC790\uD654\uD558\uB294 \uAC83\
  \uC740 \uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\uB85C\
  \ \uBCC0\uD658\uD558\uACE0 \uB098\uBA38\uC9C0\uB97C \uC18C\uBB38\uC790\uB85C \uBCC0\
  \uD658\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uC774 \uC791\uC5C5\
  \uC740 \uC785\uB825\uC744 \uC815\uADDC\uD654\uD558\uAC70\uB098 \uC81C\uBAA9, \uC774\
  \uB984 \uB4F1\uC758 \uAC00\uB3C5\uC131\uC744 \uD5A5\uC0C1\uC2DC\uD0A4\uAE30 \uC704\
  \uD574 \uB370\uC774\uD130 \uCC98\uB9AC\uC5D0\uC11C \uC77C\uBC18\uC801\uC73C\uB85C\
  \ \uC0AC\uC6A9\uB429\uB2C8\uB2E4."
lastmod: '2024-03-11T00:14:28.487497-06:00'
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC744 \uB300\uBB38\uC790\uD654\uD558\uB294 \uAC83\uC740\
  \ \uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\uB85C \uBCC0\
  \uD658\uD558\uACE0 \uB098\uBA38\uC9C0\uB97C \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\
  \uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uC774 \uC791\uC5C5\uC740\
  \ \uC785\uB825\uC744 \uC815\uADDC\uD654\uD558\uAC70\uB098 \uC81C\uBAA9, \uC774\uB984\
  \ \uB4F1\uC758 \uAC00\uB3C5\uC131\uC744 \uD5A5\uC0C1\uC2DC\uD0A4\uAE30 \uC704\uD574\
  \ \uB370\uC774\uD130 \uCC98\uB9AC\uC5D0\uC11C \uC77C\uBC18\uC801\uC73C\uB85C \uC0AC\
  \uC6A9\uB429\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
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
