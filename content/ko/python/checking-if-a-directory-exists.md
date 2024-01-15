---
title:                "디렉토리의 존재 여부 확인하기"
html_title:           "Python: 디렉토리의 존재 여부 확인하기"
simple_title:         "디렉토리의 존재 여부 확인하기"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜
누군가 디렉토리가 존재하는지 확인하기 위해 코드를 작성하는 이유는, 프로그램이 특정 디렉토리에 접근해야 할 때 해당 디렉토리가 실제로 존재하는지 확인하기 위해서입니다.

## 방법
```Python
import os

# 디렉토리 경로 설정
directory_path = '/Users/username/Documents/test'

# 디렉토리가 존재하는지 확인하는 함수
if os.path.exists(directory_path):
  print("디렉토리가 존재합니다.")
else:
  print("디렉토리가 존재하지 않습니다.")
```

위의 코드는 `os` 모듈을 사용하여 디렉토리 경로를 설정하고, `os.path.exists()` 함수를 사용하여 디렉토리의 존재 여부를 확인하는 예시입니다. 만약 디렉토리가 존재하지 않으면 `디렉토리가 존재하지 않습니다.`라는 메세지가 출력됩니다.

## 깊이있는 설명
파이썬에서 디렉토리가 존재하는지 확인하는 가장 간단한 방법은 `os.path.exists()` 함수를 사용하는 것입니다. 이 함수는 매개변수로 디렉토리 경로를 받아 해당 디렉토리가 실제로 존재하는지 여부를 불리언 값으로 반환합니다. 만약 디렉토리가 존재한다면 `True`를 반환하고, 존재하지 않는다면 `False`를 반환합니다.

때때로 디렉토리가 존재하지 않는 경우에는 새로운 디렉토리를 만들어야 할 수도 있습니다. 이를 위해서는 `os.mkdir()` 함수를 사용할 수 있습니다. 이 함수는 매개변수로 디렉토리 경로를 받고, 해당 경로에 새로운 디렉토리를 생성합니다.

그 밖에도 `os.path.isdir()` 함수를 사용하여 해당 경로가 디렉토리인지 확인하거나, `os.listdir()` 함수를 사용하여 디렉토리 내의 파일 목록을 확인할 수 있습니다.

## 참고
더 많은 파이썬 파일 및 디렉토리 작업을 알고 싶다면 아래 링크를 참고해보세요.
- [파이썬 공식 문서: os 모듈](https://docs.python.org/ko/3/library/os.html)
- [GeeksforGeeks: Python 파일 및 디렉토리 작업](https://www.geeksforgeeks.org/python-list-files-in-a-directory/)
- [점프 투 파이썬: 파일 읽고 쓰기](https://wikidocs.net/26)