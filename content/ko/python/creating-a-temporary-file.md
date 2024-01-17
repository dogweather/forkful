---
title:                "임시 파일 생성하기"
html_title:           "Python: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
임시 파일을 만드는 것은 파이썬 프로그래머들이 자주 사용하는 중요한 방법 중 하나입니다. 임시 파일은 프로그램이 실행되는 동안 일시적으로 사용되며, 필요하지 않게 되면 자동으로 삭제됩니다. 이를 통해 프로그래머들은 메모리를 절약하고 시스템 리소스를 최적화할 수 있습니다.

## How to:
파이썬에서 최신 버전인 3.8을 사용한다고 가정하겠습니다. 임시 파일을 만드는 가장 간단한 방법은 `tempfile` 모듈을 이용하는 것입니다. 아래의 코드는 임시 파일을 만들고 쓰기 위한 예제입니다.

```python
import tempfile

# 임시 파일 생성
temp = tempfile.TemporaryFile()

# 파일에 데이터 쓰기
temp.write(b"Hello, world!")

# 파일에서 데이터 읽기
temp.seek(0)
print(temp.read())

# 파일 자동 삭제
temp.close()
```

위 코드를 실행하면 `"Hello, world!"`가 콘솔에 출력되는 것을 볼 수 있습니다.

## Deep Dive:
임시 파일이 처음 등장한 것은 계산 필터링과 관련된 운영체제에서였습니다. 이전에는 프로그램이 메모리를 관리하는 것이 너무 어려워서 프로그램이 종료될 때까지 메모리를 차지해야 했습니다. 하지만 임시 파일을 사용하면 프로그램 종료 시 파일이 자동으로 제거되므로 메모리 관리가 더 쉬워졌습니다.

현재 파이썬에서 임시 파일을 생성하는 다른 방법으로는 `tempfile.mkstemp()`와 `tempfile.NamedTemporaryFile()`를 사용하는 것이 있습니다. 각각 파일 디스크립터와 임시 파일 이름을 반환합니다.

임시 파일을 생성하는 방법 뿐만 아니라, `tempfile` 모듈은 임시 디렉토리를 생성해주는 함수들도 제공합니다.

## See Also:
- [파이썬 공식 문서 - tempfile 모듈](https://docs.python.org/ko/3/library/tempfile.html)
- [RealPython - Working With Temporary Files in Python](https://realpython.com/python-tempfile/)
- [Tutorialspoint - Python - Temporary Files](https://www.tutorialspoint.com/python/python_temporary_files.htm)