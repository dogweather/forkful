---
title:                "명령 줄 인수 읽기"
html_title:           "Python: 명령 줄 인수 읽기"
simple_title:         "명령 줄 인수 읽기"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
명령 줄 인수를 읽는 것은 프로그래머들이 컴퓨터 프로그램을 사용할 때 필요한 설정이나 옵션들을 설정하기 위해 하는 것입니다. 이를 통해 프로그램 실행시에 다양한 정보를 제공할 수 있습니다. 

## 어떻게:
```python
import sys

# 명령 줄 인수를 리스트로 가져옵니다.
args = sys.argv

# 리스트의 첫 번째 인자는 프로그램 자체이므로, 두 번째 인덱스부터 읽어옵니다.
# 예를 들어, "python myprogram.py arg1 arg2" 이면 args[1]은 "arg1"이고, args[2]는 "arg2"가 됩니다.
# 일반적으로 우리는 이 인수들을 파싱(parse)하지 않고 리스트 형태 그대로 사용합니다.
```

## 깊게 들어가보면:
명령 줄 인수를 읽어오는 방식은 오래된 유닉스 시스템에 기반을 두고 있습니다. 또한 파이썬에서는 `argparse`와 같은 다른 라이브러리를 통해 조금 더 가독성 좋게 인수를 파싱할 수 있습니다. 명령 줄 인수를 사용하는 코드는 다른 프로그래머들이 쉽게 읽고 이해할 수 있도록 언제나 깔끔하게 작성해야 합니다. 

## 관련 자료:
- [Python Documentation for `sys` module](https://docs.python.org/3/library/sys.html#sys.argv)
- [RealPython's tutorial on `argparse`](https://realpython.com/command-line-interfaces-python-argparse/)