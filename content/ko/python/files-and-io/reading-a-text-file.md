---
date: 2024-01-20 17:55:05.327164-07:00
description: "How to: (\uBC29\uBC95) \uC61B\uB0A0\uC5D0\uB294 \uD30C\uC77C\uC744 \uC77D\
  \uC744 \uB54C `open()` \uD568\uC218\uB97C \uC0AC\uC6A9\uD55C \uD6C4 `close()` \uD568\
  \uC218\uB85C \uB2EB\uC544 \uC8FC\uB294 \uAC83\uC774 \uAD00\uB840\uC600\uC2B5\uB2C8\
  \uB2E4. \uD558\uC9C0\uB9CC Python 2.5 \uC774\uD6C4\uB85C `with` \uBB38\uC774 \uB3C4\
  \uC785\uB418\uBA74\uC11C \uD30C\uC77C\uC744 \uC790\uB3D9\uC73C\uB85C \uB2EB\uC744\
  \ \uC218 \uC788\uAC8C \uB418\uC5C8\uC2B5\uB2C8\uB2E4. `open()` \uD568\uC218\uC758\
  \ \uB450 \uBC88\uC9F8\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.476276-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) \uC61B\uB0A0\uC5D0\uB294 \uD30C\uC77C\uC744 \uC77D\uC744\
  \ \uB54C `open()` \uD568\uC218\uB97C \uC0AC\uC6A9\uD55C \uD6C4 `close()` \uD568\uC218\
  \uB85C \uB2EB\uC544 \uC8FC\uB294 \uAC83\uC774 \uAD00\uB840\uC600\uC2B5\uB2C8\uB2E4\
  ."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30"
weight: 22
---

## How to: (방법)
```python
# 파일 읽기 - 한 번에 모두
with open('example.txt', 'r', encoding='utf-8') as file:
    content = file.read()
    print(content)

# 파일 읽기 - 라인별로
with open('example.txt', 'r', encoding='utf-8') as file:
    for line in file:
        print(line.strip())
```

예상 출력:
```
첫 번째 줄입니다.
두 번째 줄이에요.
세 번째 줄이죠.
```

## Deep Dive (심층 탐구)
옛날에는 파일을 읽을 때 `open()` 함수를 사용한 후 `close()` 함수로 닫아 주는 것이 관례였습니다. 하지만 Python 2.5 이후로 `with` 문이 도입되면서 파일을 자동으로 닫을 수 있게 되었습니다. `open()` 함수의 두 번째 인자인 'r'은 '읽기 모드(read mode)'를 의미합니다.

파이썬에서는 라이브러리를 통한 다양한 대안도 제공합니다. 예를 들어, `pandas` 라이브러리를 사용하면 CSV나 Excel 파일도 쉽게 읽고 처리할 수 있습니다. 실행 세부 사항에 대해서는 파일의 크기, 읽는 방식(전체 또는 스트림)에 따라 메모리 사용량이나 속도가 달라질 수 있음을 기억해야 합니다.

## See Also (더 보기)
- Python 공식 문서 내 파일 읽고 쓰기: https://docs.python.org/ko/3/tutorial/inputoutput.html#reading-and-writing-files
- `pandas` 라이브러리: https://pandas.pydata.org/
- 파일 입출력에 대한 추가 튜토리얼: https://realpython.com/read-write-files-python/
