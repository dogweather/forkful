---
title:                "디렉토리 존재 여부 확인하기"
date:                  2024-01-20T14:58:22.072658-07:00
html_title:           "Fish Shell: 디렉토리 존재 여부 확인하기"
simple_title:         "디렉토리 존재 여부 확인하기"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 그리고 왜?)
디렉토리 존재 여부를 확인하는 것은, 말 그대로 파일 시스템 내에 특정 디렉토리가 실제로 존재하는지 알아보는 과정입니다. 프로그래머들은 파일을 저장하거나 읽기 전에 에러를 방지하기 위해 이 작업을 수행합니다.

## How to: (어떻게 하나요?)
```Python
import os

# 디렉토리가 존재하는지 확인
if os.path.isdir("/path/to/your/directory"):
    print("디렉토리가 존재합니다.")
else:
    print("디렉토리가 없습니다.")
```

다른 방법으로는 `pathlib` 모듈을 사용할 수 있습니다.

```Python
from pathlib import Path

# 디렉토리가 존재하는지 확인
path = Path("/path/to/your/directory")
if path.is_dir():
    print("디렉토리가 존재합니다.")
else:
    print("디렉토리가 없습니다.")
```

## Deep Dive (심층 분석)
디렉토리 존재 확인은 파일 시스템 작업에서 기본적이면서 중요한 단계입니다. 역사적으로 Unix 기반 시스템에서 파일과 디렉토리의 존재 유무 확인은 시스템의 안정성 및 보안에서 중요한 역할을 했습니다. Python의 `os` 모듈과 `pathlib` 라이브러리는 이러한 작업을 보다 쉽게 하기 위해 나중에 등장했습니다. `os.path.isdir()`는 파일 경로가 디렉토리인지 확인합니다. 그러나 Python 3에서는 `pathlib`라는 더 모던 라이브러리가 등장하여 객체 지향적 접근을 제공합니다. 두 방법 모두 내부적으로는 운영체제의 파일시스템 API를 호출하여 결과를 얻습니다.

## See Also (추가 정보)
- [Python `os` module documentation](https://docs.python.org/3/library/os.html)
- [Python `pathlib` module documentation](https://docs.python.org/3/library/pathlib.html)
- [Stack Overflow: How can I check if a directory exists in Python?](https://stackoverflow.com/questions/8933237/how-can-i-check-if-a-directory-exists-in-python)
