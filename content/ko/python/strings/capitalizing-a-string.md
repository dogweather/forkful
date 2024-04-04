---
changelog:
- 2024-04-04 - dogweather - edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:34.962078-07:00
description: "\uC5B4\uB5BB\uAC8C \uD558\uB098: #."
lastmod: '2024-04-04T00:26:59.395145-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
weight: 2
---

## 어떻게 하나:

### 파이썬의 내장 메소드 사용:
파이썬은 문자열을 쉽게 처리할 수 있는 `.capitalize()`라는 내장 메소드가 있습니다.

```python
my_string = "hello world"
capitalized_string = my_string.capitalize()
print(capitalized_string)
```
**출력:**
```
Hello world
```

여기 제가 이 사이트를 구축하기 위해 사용한 맞춤형 `capitalize()` 함수가 있습니다. **HTML**과 같은 특수 단어들이 항상 대문자로 유지되도록 해야 했습니다. 이는 [doctests](https://docs.python.org/3/library/doctest.html)를 활용한 예시도 보여줍니다:

```python
def capitalize(string: str) -> str:
    """
    문자열의 첫 글자를 대문자로 만듭니다.
    "HTML"과 같은 특수 케이스를 처리합니다.

    >>> capitalize("this is html, csv, xml, and http (no REPL).")
    'This is HTML, CSV, XML, and HTTP (no REPL).'

    >>> capitalize("this is json, VBA, an IDE, and yaml in the CLI.")
    'This is JSON, VBA, an IDE, and YAML in the CLI.'
    """
    return (
        string
            .capitalize()
            .replace('cli',  'CLI')
            .replace('csv',  'CSV')
            .replace('html', 'HTML')
            .replace('http', 'HTTP')
            .replace('ide',  'IDE')
            .replace('json', 'JSON')
            .replace('repl', 'REPL')
            .replace('vba',  'VBA')
            .replace('xml',  'XML')
            .replace('yaml', 'YAML')
    )

```




### 여러 단어를 처리할 때:
문자열의 각 단어가 대문자로 시작하길 원하는 시나리오(예: 제목)에서는 `.title()` 메소드를 사용할 수 있습니다.

```python
my_title = "python programming essentials"
title_case = my_title.title()
print(title_case)
```
**출력:**
```
Python Programming Essentials
```

### 서드 파티 라이브러리 사용하기:
파이썬의 표준 라이브러리는 기본적인 문자열 대문자 변환을 위한 장비를 갖추고 있지만, `textblob`과 같은 라이브러리는 특히 자연어 처리를 위해 더 미묘한 제어를 제공할 수 있습니다.

먼저, `textblob`이 설치되어 있는지 확인하세요:
```bash
pip install textblob
```

그런 다음, `textblob`을 사용해 문자열을 대문자로 만들어 보세요. `textblob`의 대문자화 기능은 사용 맥락에 따라 다르게 작동할 수 있다는 점을 유념하세요:

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

`capitalize()` 및 `title()` 메소드가 보편적으로 유용하지만, `textblob`과 같은 라이브러리를 활용하면 특정 응용 프로그램에 대해 추가적인 유연성을 제공할 수 있습니다.
