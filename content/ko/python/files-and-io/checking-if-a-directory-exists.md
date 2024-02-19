---
aliases:
- /ko/python/checking-if-a-directory-exists/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:23.655155-07:00
description: "Python\uC5D0\uC11C \uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\
  \uB294\uC9C0 \uD655\uC778\uD558\uB294 \uAC83\uC740 \uD30C\uC77C\uC744 \uC77D\uAC70\
  \uB098 \uC4F0\uB294 \uB4F1\uC758 \uC791\uC5C5\uC744 \uC218\uD589\uD558\uAE30 \uC804\
  \uC5D0 \uD30C\uC77C \uC2DC\uC2A4\uD15C\uC5D0\uC11C \uD3F4\uB354\uC758 \uC874\uC7AC\
  \uB97C \uAC80\uC99D\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uC774\uB97C \uD1B5\uD574 `FileNotFoundError`\uC640 \uAC19\uC740\
  \ \uC624\uB958\uB97C \uD53C\uD558\uACE0, \uB514\uB809\uD1A0\uB9AC\uC640 \uC0C1\uD638\
  \uC791\uC6A9\uC744 \uC2DC\uB3C4\uD560 \uB54C\u2026"
lastmod: 2024-02-18 23:09:05.647312
model: gpt-4-0125-preview
summary: "Python\uC5D0\uC11C \uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\
  \uC9C0 \uD655\uC778\uD558\uB294 \uAC83\uC740 \uD30C\uC77C\uC744 \uC77D\uAC70\uB098\
  \ \uC4F0\uB294 \uB4F1\uC758 \uC791\uC5C5\uC744 \uC218\uD589\uD558\uAE30 \uC804\uC5D0\
  \ \uD30C\uC77C \uC2DC\uC2A4\uD15C\uC5D0\uC11C \uD3F4\uB354\uC758 \uC874\uC7AC\uB97C\
  \ \uAC80\uC99D\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC774\uB97C \uD1B5\uD574 `FileNotFoundError`\uC640 \uAC19\uC740 \uC624\
  \uB958\uB97C \uD53C\uD558\uACE0, \uB514\uB809\uD1A0\uB9AC\uC640 \uC0C1\uD638\uC791\
  \uC6A9\uC744 \uC2DC\uB3C4\uD560 \uB54C\u2026"
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
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
