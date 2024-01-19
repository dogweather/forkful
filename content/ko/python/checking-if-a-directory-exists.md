---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "C#: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

디렉토리가 존재하는지 확인하는 것은 파일시스템에서 특정 디렉토리의 존재 여부를 확인하는 프로그래밍 작업입니다. 데이터를 저장하거나 불러오는 등의 작업을 시작하기 전에 데이터의 추적 및 손실을 피하기 위해 이것을 수행합니다.

## 어떻게 할까?

아래 파이썬 코드는 'os' 모듈을 사용하여 디렉토리가 존재하는지 확인하는 예입니다.

```Python
import os

def check_directory_exists(dir):
  return os.path.isdir(dir)

# 사용 예
print(check_directory_exists('./test_directory')) 
```
이 코드를 실행하면, 'test_directory'가 존재하는 경우에는 True를 반환하고 그렇지 않은 경우에는 False를 반환합니다.

## 깊은 이해

디렉토리가 존재하는지 확인하는 작업은 오래된 프로그래밍 요소 중 하나로, 다양한 방법으로 실행될 수 있습니다. Python이 제공하는 'os.path' 모듈 외에도 'pathlib' 모듈 역시 사용할 수 있습니다.

'os.path'는 파일/디렉토리 관련 작업에 사용되는 오래된 모듈이지만 'pathlib'은 Python 3.4 버전에서 도입된 새로운 방식입니다.

'pathlib'를 사용한 디렉토리 존재 확인 방법은 아래와 같습니다.

```Python
from pathlib import Path

def check_directory_exists(dir):
  return Path(dir).is_dir()

# 사용 예
print(check_directory_exists('./test_directory')) 
```

위 코드 역시 'test_directory'가 존재하는 경우에는 True를 반환하고 존재하지 않는 경우에는 False를 반환합니다.

## 참고 자료

디렉토리 존재 확인 작업에 관해 더 깊이 알아보려면 아래의 참고 자료를 참조하십시오.

1. Python 공식 문서의 'os.path' 모듈: https://docs.python.org/3/library/os.path.html
2. Python 공식 문서의 'pathlib' 모듈: https://docs.python.org/3/library/pathlib.html
3. 모듈 사용법에 대한 Stack Overflow 토론: https://stackoverflow.com/questions/82831/how-do-i-check-whether-a-file-exists-without-exceptions