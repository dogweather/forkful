---
title:                "디렉토리가 존재하는지 확인하기"
aliases:
- /ko/python/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:23.655155-07:00
model:                 gpt-4-0125-preview
simple_title:         "디렉토리가 존재하는지 확인하기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
Python에서 디렉토리가 존재하는지 확인하는 것은 파일을 읽거나 쓰는 등의 작업을 수행하기 전에 파일 시스템에서 폴더의 존재를 검증하는 것입니다. 프로그래머들은 이를 통해 `FileNotFoundError`와 같은 오류를 피하고, 디렉토리와 상호작용을 시도할 때 애플리케이션이 신뢰성 있게 동작하며 충돌하지 않도록 합니다.

## 방법:
Python은 `os` 및 `pathlib` 모듈을 사용하여 디렉토리의 존재 여부를 확인하는 기본적인 방법을 제공합니다. 다음은 두 가지 방법에 대한 예시입니다:

### `os` 모듈 사용
```python
import os

# 디렉토리 경로 지정
dir_path = "/path/to/directory"

# 디렉토리가 존재하는지 확인
if os.path.isdir(dir_path):
    print(f"디렉토리 {dir_path}가(이) 존재합니다.")
else:
    print(f"디렉토리 {dir_path}가(이) 존재하지 않습니다.")
```

### `pathlib` 모듈 사용
```python
from pathlib import Path

# 디렉토리 경로 지정
dir_path = Path("/path/to/directory")

# 디렉토리가 존재하는지 확인
if dir_path.is_dir():
    print(f"디렉토리 {dir_path}가(이) 존재합니다.")
else:
    print(f"디렉토리 {dir_path}가(이) 존재하지 않습니다.")
```

### 서드파티 라이브러리
Python의 표준 라이브러리만으로도 디렉토리가 존재하는지 확인하는 것에 충분하지만, 버전 간의 일관성 또는 추가 기능을 위해 `pathlib2`와 같은 라이브러리를 대안으로 사용할 수 있습니다.

***참고:*** 최신 Python 버전에 따르면, 대부분의 사용 사례에 있어 `pathlib`이 충분히 강력하므로 이 특정 작업에 대한 서드파티 라이브러리는 덜 필요해졌습니다.
